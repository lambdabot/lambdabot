lambdabot ![Build Status](https://travis-ci.org/mokus0/lambdabot.png)
===============

Lambdabot is an IRC bot written over several years by those on freenode's #haskell [IRC channel](http://www.haskell.org/haskellwiki/IRC_channel). It also operates in an offline mode as a Haskell development tool, and embedded as an extension to ghci.

PREREQUISITES
-------------

You'll need GHC >= 7.4.  cabal-install is highly recommended.

External executable dependencies:

- `aspell` for the "spell" spell-checking plugin.  This is not a Haskell program, but is available in pretty much all Linux, BSD and Mac OS package managers.
- `bf` for interpreting brainfuck programs.  This is a provided by the "brainfuck" package on Hackage.
- `djinn` for the "djinn" plugin, which tries to find Haskell functions matching arbitrary types.  Available on Hackage.
- `ghc` and `mueval` for the "eval" plugin, which evaluates Haskell expressions in chat (when prefixed with "> "; e.g. `> 1 + 1`).  GHC is available from haskell.org (the Haskell Platform is recommended).  Mueval is available on Hackage.
- `hoogle` for the "hoogle" plugin, which provides a command for searching Haskell APIs.  Available from Hackage.
- GNU talk-filters (optional) for the "filters" plugin.  Available via most package managers, I believe.
- `unlambda` for executing unlambda programs.  Available on Hackage.

Some of these dependencies (those with corresponding hackage libraries) will be installed by cabal, but not all of them will.  In all cases, cabal does not actually track the executables so if they have previously been installed and deleted on your system (without unregistering the matching library), you will have to manually reinstall them.

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

CONFIGURING
===========

Lambdabot uses an extensible configuration system which allows plugins to define their own configuration variables.  The lambdabot core system defines several, listed in the module `Lambdabot.Config.Core`.  The default `lambdabot` executable provides a command-line interface to set some of the most common ones, but currently the only way to set others is to define your own executable (which you must currently do anyway to change the default set of modules).

When doing so, configuration is passed in to the `lambdabotMain` function as a list of bindings.  Configuration variables are bound using the `:=>` operator (actually the data constructor of the `DSum` type), for example:

    ghcBinary :=> "ghc-7.4.2"

So a typical custom lambdabot executable might look something like:

    module MyBot where
    
    import Lambdabot.Main
    {- import your plugins here -}
    
    main = lambdabotMain myPlugins 
        [ configKey  :=> value
        , anotherKey :=> anotherValue
        ]

BUGS
====

Bug reports, patches, new modules etc., open issues on GitHub or contact:

        James Cook <mokus@deepbondi.net>
        aka mokus on #haskell

REPOSITORY
==========

    git clone https://github.com/mokus0/lambdabot

CONTRIBUTING
============

Send pull requests to mokus0 on github. Add yourself to the AUTHORS
file if you haven't already.
