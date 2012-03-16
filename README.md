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

WARNING: uses Safe Haskell for @eval, etc., does not turn on -fpackage-trust, so make sure that you don't have any packages installed that are incorrectly marked "Trustworthy".

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

    git clone https://github.com/mokus0/lambdabot

CONTRIBUTING
============

Send pull requests to mokus0 on github. Add yourself to the AUTHORS
file if you haven't already.
