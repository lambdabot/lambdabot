      The Type Module - another progressive plugin for lambdabot
     >>-------------------------------------------------------->>
      $Id: TypeModule.lhs,v 1.4 2003/07/29 13:03:02 eris Exp $

> module TypeModule(typeModule, theModule) where

> import Posix (popen)
> import Text.Regex
> import Maybe (mapMaybe, fromMaybe)
> import Control.Monad.Trans (liftIO)
> import IRC -- (Module(..), IRC(..), ircPrivmsg)
> import Util



      Greetings reader,

      whether you're a regular follower of the series or dropping in for
      the first time, let me present for your pleasure the Type Module:

      One thing we enjoy on #haskell is throwing function types at each
      other instead of spelling out tiresome monologue about arguments
      or return values. Unfortunately such a toss often involves a local
      lookup of the type signature in question because one is seldom
      sure about the actual argument order.

      Well, what do you know, this plugin enables lambdabot to automate
      that lookup for you and your fellow lambda hackers.


      In accordance with the KISS principle, the plan is to delegate all
      the hard work! To get the type of foo, pipe

> command foo = ":t " ++ foo
>
> infocommand foo = ":i " ++ foo

      into hugs and send any line matching

> signature_regex
>   = mkRegexWithOpts
>     ".*?^\\*?[A-Z][\\._a-zA-Z0-9]*(\\*?[A-Z][_a-zA-Z0-9]*)*> (.*)^\\*?[A-Z][\\._a-zA-Z0-9]*(\\*?[A-Z][_a-zA-Z0-9]*)*> "
>     False True

      through IRC.


      To get any signature line from the hugs output, split it into lines,
      match each against the regex, and take the last substring match from
      each successfull match.

> extract_signatures output
>   = matchRegex signature_regex output
>   -- = map last.mapMaybe (matchRegex signature_regex) $ lines output


      With this the command handler can be easily defined using popen:

> find_type :: String -> IO [String]
> find_type expr =
>      do 
>      let bits = split "." expr
>          (args,item) = (\ (x:xs) -> (join "." $ reverse xs,x)) $ reverse bits
>      (output, _, _) <- popen "ghci" [] (Just (command expr))
>      return (fromMaybe ["bzzt"] $ matchRegex signature_regex output)
>      --return output
>      --mapM_ (ircPrivmsg src) (extract_signatures output)
 

> find_info :: String -> IO [String]
> find_info expr =
>      do 
>      let bits = split "." expr
>          (args,item) = (\ (x:xs) -> (join "." $ reverse xs,x)) $ reverse bits
>      (output, _, _) <- popen "ghci" [] (Just (infocommand expr))
>      return (fromMaybe ["bzzt"] $ matchRegex signature_regex output)

      And thus the plugin:

> newtype TypeModule = TypeModule ()

> theModule = MODULE typeModule

> typeModule = TypeModule ()

> instance Module TypeModule where
>    moduleName   _ = return "type"
>    moduleSticky _ = False
>    commands     _ = return ["type","info"]
>    process _ _ src "type" expr = do strs <- liftIO (find_type expr)
>                                     mapM_ (ircPrivmsg src) strs
>    process _ _ src "info" expr = do strs <- liftIO (find_info expr)
>                                     mapM_ (ircPrivmsg src) strs




                                          Till next time,               :.
                                                                         .
                                          .pesco hamburg 2003-04-05.    |
                                                                       /_\   
