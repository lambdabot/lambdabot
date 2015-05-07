--
-- | Generate the initial @index state
--
import Text.HTML.TagSoup
import Data.List

main = interact (unlines . go [] . parseTags)

go mods (TagOpen "td" [("class","src")] : TagText ident : xs) =
    nub mods ++ ["" | not (null mods)] ++ ident : go [] xs
go mods (TagOpen "td" [("class","module")] : xs) =
    let (ys, zs) = span (not . isTagCloseName "td") xs in
    go (mods ++ words (filter (/= ',') (innerText ys))) zs
go mods (x : xs) = go mods xs
go mods [] = nub mods ++ ["" | not (null mods)]
