module HelloModule where
-- 	$Id: HelloModule.hs,v 1.5 2003/07/29 13:03:02 eris Exp $
import IRC

newtype HelloModule = HelloModule ()

theModule :: MODULE
theModule = MODULE helloModule

helloModule :: HelloModule
helloModule = HelloModule ()

instance Module HelloModule where
    moduleName   _ = return "hello"
    moduleHelp   _ _ = return "hello world plugin"
    moduleSticky _ = False
    commands     _ = return ["hello","goodbye"]
    process      _ _ target _ rest = ircPrivmsg target ("Hello world. " ++ rest)

