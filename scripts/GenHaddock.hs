--
-- | Generate the initial @index state
--
import System.Cmd
import System.Directory
import Data.List

main :: IO ()
main = do
    system $ "wget -nc "++
      "http://haskell.org/ghc/docs/latest/html/libraries/doc-index.html"
    dir   <- getCurrentDirectory
    let filesdir = dir ++ "/haskell.org/ghc/docs/latest/html/libraries/"
    files <- getDirectoryContents filesdir
    let docfiles = filter ("doc-index-"`isPrefixOf`) files
    assocs <- fmap concat $ mapM getAssocs $ map (filesdir++) docfiles
    writeFile "../State/haddock" $ unlines $ map show assocs


getAssocs :: FilePath -> IO [(String,[String])]
getAssocs file = do
  cont <- lines `fmap` readFile file
  let isKey str = "><TD CLASS=\"indexentry\"" `isPrefixOf` str
      sections = tail $ groupBy (const $ not . isKey) cont
  return $ do
    s <- sections
    let key = clean $ (reverse . drop 4 . reverse . drop 1) (s !! 1)
    let values = map (reverse . drop 3 . reverse . drop 1) $
          filter ("</A"`isSuffixOf`) s
    return (key,values)

clean :: String -> String
clean ('&':'a':'m':'p':';':xs) = '&':clean xs
clean ('&':'l':'t':';':xs) = '<':clean xs
clean ('&':'g':'t':';':xs) = '>':clean xs
clean (x:xs) = x:clean xs
clean [] = []
