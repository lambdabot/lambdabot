# About Lambdabot

Lambdabot is an IRC bot written over several years by those on the Freenode
`#haskell` IRC channel.

It operates as a command line tool, embedded in an editor, embedded in GHCi,
via internet relay chat and on the web.

## Installation

The easiest way to install `lambdabot` is from hackage, by the following
commands:

    cabal install --constraint 'transformers installed' lambdabot djinn
    hoogle data

The second command fetches the data for Lambdabot's `@hoogle` command, which
is a thin wrapper around the [`hoogle`](https://www.haskell.org/hoogle/)
command line tool. Invoking `lambdabot` will (hopefully) display a
`lambdabot>` prompt.

It is also possible to install lambdabot in a sandbox. In that case, use
`cabal exec lambdabot` to launch lambdabot.

## Files

Some lambdabot modules maintain state. The state is stored in `./State/`
if that directory exits; otherwise, it will end up on `~/.lambdabot/State/`.
State files of particular interest are:

 * `Pristine.hs` and `L.hs`: Environment for running Haskell code.
   The command `@let` adds new definitions to `L.hs`, whereas `@undefine`
   copies `Pristine.hs` to `L.hs`.
 * `offlinerc`: This file contains a history of commands typed into
   lambdabot's tty interface.

## Customization

The state file `Pristine.hs` defines the environment in which Haskell code
is run. To customize lambdabot's modules, unpack the lambdabot package

    cabal unpack lambdabot; cd lambdabot-<version>

You can then edit `src/Modules.hs` to configure the loaded modules.

## Using Lambdabot

Lambdabot has a number of modules, most of which provide several commands.
Type `@listmodules` to obtain a list of module names, and then
`@list <module>` to list a module's commands. Executing `@help command`
displays a short description of the command.

The following sample session demonstrates some useful lambdabot commands.

    lambdabot> > sum [1..10]
     55
    lambdabot> @let foo = 42
    lambdabot> > product [1..foo]
     1405006117752879898543142606244511569936384000000000
    lambdabot> @undefine
        Not in scope:‘foo’
        Perhaps you meant ‘Data.Traversable.for’ (imported from Data.Traversable)
    lambdabot> @type map
    (a -> b) -> [a] -> [b]
    lambdabot> @djinn (b -> c) -> (a -> b) -> a -> c
    f a b c = a (b c)
    lambdabot> @pl \x y z -> y z x
    flip flip
    lambdabot> @unpl flip flip
    (\ b c f -> c f b)
    lambdabot> @undo do x <- step1; step2; step3
    step1 >>= \ x -> step2 >> step3

## Further Information

- [lambdabot](https://github.com/lambdabot/lambdabot) on github
- [lambdabot](https://wiki.haskell.org/Lambdabot) on the Haskell wiki (outdated)
- [GOA: GHCI integration](https://wiki.haskell.org/GHC/GHCi#GHCi_on_Acid) (Haskell Wiki)
