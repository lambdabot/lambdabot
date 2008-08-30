--
-- | Hello world plugin
--
module Plugin.Hello where

$(plugin "Hello")

instance Module Hello () where
    moduleCmds _  = ["hello","goodbye"]
    moduleHelp _  = "hello/goodbye <arg>. Simplest possible plugin"
    process_ _ xs = return ["Hello world. " ++ xs]

