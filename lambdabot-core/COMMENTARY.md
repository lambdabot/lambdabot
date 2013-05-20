COMMENTARY ON LAMBDABOT
=======================

Basic concepts
------------

Lambdabot's functionality is provided by a "core" that provides message routing and some basic infrastructure for several "plugins", which implement specific user-facing behaviors.  The core makes some assumptions about the nature of those behaviors.  The following, in no particular order, are some of the central concepts of the lambdabot design:

- Messages.  An IRC-centric representation of a chat message.  Includes information about the sender and addressee, as well as the "kind" (PRIVMSG, CTCP PING, etc.) and the text of the message.

- Servers.  A "server" is a simplified model of the kind of user interaction provided by an IRC server.  It accepts messages from a user, feeds them into the lambdabot core, and forwards messages from the core back to users.  There are currently 2 types of server defined - IRC connections and the "OfflineRC" REPL.

- Plugins (sometimes called "modules").  A plugin is described by a record of type `Module st`, where `st` is the type of the plugin's internal state.  The plugin is able to define functions to initialize, serialize, deserialize, and clean-up that state, and may additionally define "commands" and "contextual" behaviors, both defined below.

- Commands.  A plugin defines zero or more commands, which are keywords users can use in chat (prefixed by a sigil, '@' or '?' by default) to interact with the plugin.

- Contextual behaviors.  Every time a chat message is received that isn't processed as a command, plugins are offered a chance to process and respond to it.

- Callbacks.  Plugins can register callbacks to react to non-chat messages such as nick changes, join/part events, etc.

User code execution
--------------------

Lambdabot provides the ability to execute user-supplied Haskell code.  This is obviously a somewhat risky thing to do.  We use 2 main mechanisms to mitigate this risk:

- Safe Haskell.  The "Safe Haskell" GHC extension tracks usage of features such as `unsafePerformIO` and the FFI (among others) which subvert the type system.  The full details are available in the GHC documentation.  By default, lambdabot only trusts a small set of common libraries.  The user may override this set on the command-line.

- Sandboxing and resource limits.  The `mueval` program implements timeouts and resource limits to keep user-provided code from hogging the system.  Lambdabot uses `mueval` to run code

Monads
------

Lambdabot uses a few different monads/transformers to encapsulate state and provide additional context for various subsystems.  They are:

- LB.  The `LB` monad is the base environment for all operations that depend on or affect the core system.  These are mainly things like loading or unloading plugins, registering servers or callbacks, etc.

- ModuleT.  The `ModuleT` monad transformer is used for all code provided by a plugin.  It provides access to the plugin's name and current state.

- CmdT. The `CmdT` monad transformer is used for implementing commands within modules, and provides access to information such as the name the command was invoked as, the message that triggered the command, and information about that message such as the sender and addressee.


Configuration system
--------------------

Code in all the monads above has access to the configuration system, which is a typed key-value store that is initialized at startup and thenceforth immutable.  The `getConfig` function provides access to the value associated with any config key.  New keys can be defined using the `config` Template Haskell function - see `Lambdabot.Config` for the interface and `Lambdabot.Config.Core` for some examples.

To set a config value, add a binding to the configuration parameter of `lambdabotMain`.  For example:

    module MyBot where
    
    import Lambdabot.Main
    {- import your plugins here -}
    
    main = lambdabotMain myPlugins 
        [ configKey  :=> value
        , anotherKey :=> anotherValue
        ]

Any key not listed will be assigned the default value specified in its original declaration.

Source layout and module heirarchy
----------------------------------

- src/
-- Lambdabot.Compat.*

Modules supporting backward-compatibility with older versions of lambdabot (mostly functions to read old serialization formats)

-- Lambdabot.Config.*

Currently only "Core", this module defines the configuration keys for the lambdabot core system.  Packages providing additional plugins are encouraged to use this namespace (or the module in which they are used, if they prefer) for additional configuration key definitions.  The configuration system is described above.

-- Lambdabot.Plugin.*

As the name suggests, all lambdabot plugins are defined in modules under this namespace.

-- Lambdabot.Util.*

Utility functions for use by lambdabot plugins.  This is not considered a stable public interface; I am actively working on eliminating them in favor of external libraries that provide equivalent functionality.

-- Lambdabot, Lambdabot.*

The core lambdabot system.  Like the Util namespace, these are not especially stable right now.

- main/

Defines the main lambdabot executable, usable either as-is or as a template for a customized executable.

