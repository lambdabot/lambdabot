susie-lambdabot
=============== 

This is a fork of lambdabot which was developed for use on my family IRC server (where it runs as a bot named "susie").


PREREQUISITES
-------------

You'll need GHC >= 7.4.  cabal-install is highly recommended.

Note: If you want Lambdabot to be able to evaluate expressions
(e.g., "> 1 + 1" evaluates to "2") then you'll need mueval installed.

RUNNING
=======

Lambdabot can be installed system-wide or per user, but currently the lambdabot binary makes certain assumptions about what directory it is being run in & where files it needs can be found. (This is the subject of future work.)

Your best bet is currently to read the code and see what it does, and decide for yourself whether that's what you want.

OFFLINE MODE
------------

    lambdabot

CONNECTING
----------

    lambdabot -e 'rc online.rc'

SSL MODE (with stunnel)
-----------------------

append the following to your stunnel.conf:

    client = yes
    [irc]
    accept = 6667
    connect = ssl-irc-server.org:6667

and edit online.rc to use localhost as server, then restart the stunnel
server and restart lambdabot with:

    ./lambdabot -e 'rc online.rc'

SCRIPTS
-------

    The scripts directory contains some shell scripts for Vim editor support
    They are self-explanatory

BUGS
====

Bug reports, patches, new modules etc, contact:

        James Cook <mokus@deepbondi.net>
        aka mokus on #haskell

REPOSITORY
==========

There is currently no public repository.  I'll probably convert to a git repo and put it up on github eventually though.

CONTRIBUTING
============

Use 'darcs send' to submit patches to mokus. Add yourself to the AUTHORS
file if you haven't already.
