import System
import LJT
import LJTParse

main :: IO ()
main = do
    args <- getArgs
    let testfiles =
	    case args of
		[a] -> a
		_ -> "testfiles.all"
    putStrLn $ "Tests start: " ++ testfiles
    stests <- readFile testfiles
    let tests :: [(Bool, String)]
        tests = read stests
    ns <- mapM runTest tests
    let fails = sum ns
    if fails == 0 then
        putStrLn "Tests done!"
     else do
	putStrLn $ "Tests done, " ++ show fails ++ " failed"
	exitWith (ExitFailure 1)

runTest :: (Bool, String) -> IO Int
runTest (expected, fileName) = do
    putStrLn $ "file " ++ fileName ++ "..."
    file <- readFile fileName
    let f = parseLJT file
	e = provable f
    if e == expected then do
	putStrLn "   passed"
	return 0
     else do
	putStrLn "   failed"
	putStrLn $ show (e, f)
	return 1
