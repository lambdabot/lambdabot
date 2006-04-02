--
-- | Hello world plugin
-- 
-- Illustrates the use of our preprocessor, dsl.lhs to generate the
-- module syntax for us.
--
PLUGIN(Hello)
    moduleCmds _  = ["hello","goodbye"]
    moduleHelp _  = "hello/goodbye <arg>. Simplest possible plugin" 
    process_ _ xs = return ["Hello world. " ++ xs]

