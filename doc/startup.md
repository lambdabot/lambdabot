# Lambdabot startup

## Responsibilities

- `lambdabot:Main` takes care of parsing arguments
- `lambdabot:Modules` provides the list of modules
- `lambdabot-core:Lambdabot.Main` and `lambdabot-core:Lambdabot.Bot` take care of module initialization
- `lambdabot-core:Lambdabot.Monad` keeps track of module commands
- after initialization, frontend modules take care of actually running commands

## Startup control flow

```
lambdabot:Main.main
+- lambdabot:Modules.modulesInfo --> list of <module>
`- lambdabot-core:Lambdabot.Main.lambdabotMain
   `- lambdabot-core:Lambdabot.Main.lambdabotRun
      `- lambdabot-core:Lambdabot.Main.withModules
         `- lambdabot-core:Lambdabot.Main.withModule <module>
            `- lambdabot-core:Lambdabot.Bot.ircLoadModule
               +- lambdabot-core:Lambdabot.State.readGlobalState <module>
               +- moduleInit <module>
               `- lambdabot-core:Lambdabot.Monad.registerCommands =<< moduleCmds <module>
```

## Frontend modules

`lambdabot-core:Lambdabot.Plugin.Core.OfflineRC`
- executes startup commands from the command line (`-e` flag)
- the `rc` executes commands from files
- the `offline` command executes commands from the terminal
  - cf. `lambdabot.core:Lambdabot.Plugin.Core.OfflineRC.replLoop`
  - this is the default startup command

`lambdabot-irc-plugins:Lambdabot.Plugin.IRC.IRC`
- provides the `irc-connect` command
- cf. `lambdabot-irc-plugins:Lambdabot.Plugin.IRC.IRC.online`
