#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


//OO in C
struct tuple_2_int_i {
  int i1;
  int i2;
  int i3;
  pid_t pid;
};

typedef struct tuple_2_int_i tuple_2_int;

int get_i1(tuple_2_int * i)
{
  return i->i1;
}

int get_i2(tuple_2_int * i)
{
  return i->i2;
}

int get_i3(tuple_2_int * i)
{
  return i->i3;
}

int get_pid(tuple_2_int * i)
{
  return i->pid;
}

void delete_tuple_2_int(tuple_2_int * i)
{
  free(i);
}

tuple_2_int * launch(char * progName, char ** arguments)
{
  int inpipes[2];
  int outpipes[2];
  int errorpipes[2];
  pid_t pid;

  pipe(inpipes);
  pipe(outpipes);
  pipe(errorpipes);
  pid = fork();
  if (pid == (pid_t) 0 ) {
    /* Child process */
    dup2(inpipes[0], STDIN_FILENO);
    dup2(outpipes[1], STDOUT_FILENO);
    dup2(errorpipes[1], STDERR_FILENO);
    close(outpipes[0]);
    close(outpipes[1]);
    close(inpipes[0]);
    close(inpipes[1]);
    close(errorpipes[0]);
    close(errorpipes[1]);
    /* sleep(1); */
    execvp(progName, arguments);
    printf("There was a problem");
    perror ("execvp");
    exit (1);
  } else if (pid < (pid_t) 0) {
    /* There seems to be a problem */
  } else {
    /* Parent process */
    tuple_2_int * it = malloc(sizeof (tuple_2_int));
    it->i1 = inpipes[1];
    it->i2 = outpipes[0];
    it->i3 = errorpipes[0];
    it->pid = pid;
    close(inpipes[0]);
    close(outpipes[1]);
    close(errorpipes[1]);
    /* The following three lines ensure that the fd's are closed
       when exec finishes. */
    fcntl(inpipes[1], F_SETFD, FD_CLOEXEC);
    fcntl(outpipes[0], F_SETFD, FD_CLOEXEC);
    fcntl(errorpipes[0], F_SETFD, FD_CLOEXEC);
    return it;
  }
}

int wait_for_it(tuple_2_int * tuple)
{
  int wstat;

  while (waitpid(tuple->pid, &wstat, 0) < 0) {
    if (errno != EINTR) {
      return -1;
    }
  }
  if (WIFEXITED(wstat)) {
    return WEXITSTATUS(wstat);
  } else if (WIFSIGNALED(wstat)) {
    errno = EINTR;
  } else {
    /* This should never happen */
  }
  return -1;
}
