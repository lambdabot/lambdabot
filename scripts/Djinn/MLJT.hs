--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
import IO
import System
import LJTFormula
import LJTParse
import MJ

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    args <- getArgs
    file <-
	    case args of
		[a] -> readFile a
		_ -> hGetContents stdin
    let form = parseLJT file
--	pr = provable form
--	cpr = provable (fnot (fnot form))
	mpr = take 25 $ prove False [] form
    print form
--    putStrLn $ "Classical " ++ show cpr
--    putStrLn $ "Intuitionistic " ++ show pr
--    putStrLn $ show mpr
    case mpr of
	[] -> return ()
	terms -> do
	    putStrLn $ "proof : " ++ show form
	    putStrLn $ unlines (map (("proof = " ++) . show) terms)
