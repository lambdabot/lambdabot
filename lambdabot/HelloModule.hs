module HelloModule where
-- 	$Id: HelloModule.hs,v 1.5 2003/07/29 13:03:02 eris Exp $	
import IRC

newtype HelloModule = HelloModule ()

theModule = MODULE helloModule
helloModule = HelloModule ()

instance Module HelloModule where
    moduleName   m = return "hello"
    moduleSticky m = False
    commands     m = return ["hello","goodbye"]
    process      m _ target cmd rest
      = ircPrivmsg target ("Hello world. " ++ rest)

