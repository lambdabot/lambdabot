--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
import IO
import System
import LJT
import LJTParse

main :: IO ()
main = do
    args <- getArgs
    file <-
	    case args of
		[a] -> readFile a
		_ -> hGetContents stdin
    let form = parseLJT file
	pr = provable form
	cpr = provable (fnot (fnot form))
	mpr = case prove False [] form of [] -> Nothing; p:_ -> Just p
    print form
    putStrLn $ "Classical " ++ show cpr
    putStrLn $ "Intuitionistic " ++ show pr
    putStrLn $ show mpr
    case mpr of
	Nothing -> return ()
	Just term -> do
	    putStrLn $ "proof : " ++ show form
	    putStrLn $ "proof = " ++ show term
