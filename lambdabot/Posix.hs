module Posix where
import System.IO
import System.Process

type ProcessID = ProcessHandle

popen :: FilePath -> [String] -> Maybe String -> IO (String,String,ProcessID)
popen file args minput = do
    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing
    case minput of
        Just input -> hPutStr inp input
        Nothing -> return ()
    output <- hGetContents out
    errput <- hGetContents err
    return (output,errput,pid)
