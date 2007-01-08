module VariableExpansion (expand) where

import Language.Haskell.TH

data Token1 = LitChar Char
           | ShowVar1 String
           | StrVar1 String
             deriving (Eq, Show)

data Token2 = LitStr String
           | ShowVar2 String
           | StrVar2 String
             deriving (Eq, Show)

splitAtElem :: Eq a => [a] -> a -> ([a], [a])
splitAtElem l e = splitAtPred l (e ==)

-- splitAtPred [1,2,3,4] (==3)
--   ==> ([1,2],[3,4])
splitAtPred :: [a] -> (a -> Bool) -> ([a], [a])
splitAtPred (x:xs) p
    | p x        = ([], x:xs)
    | otherwise  = let (a, b) = splitAtPred xs p
                       in (x:a, b)
splitAtPred [] _ = ([], [])

parse :: String -> Q [Token1]
parse ('$':'$':s) = do l <- parse s
                       return (LitChar '$' : l)
parse ('%':'%':s) = do l <- parse s
                       return (LitChar '%' : l)
parse ('$':'{':s) = do (name, restToks) <- parseVar s
                       return (ShowVar1 name : restToks)
parse ('%':'{':s) = do (name, restToks) <- parseVar s
                       return (StrVar1 name : restToks)
parse ('$':_:_)   = fail "Error parsing string to expand"
parse ('%':_:_)   = fail "Error parsing string to expand"
parse (c:s)       = do l <- parse s
                       return (LitChar c : l)
parse []          = return []

parseVar :: [Char] -> Q ([Char], [Token1])
parseVar s =
    case splitAtElem s '}' of
      (_, []) -> fail "Error parsing string to expand"
      (name, '}':rest) -> do l <- parse rest
                             return (name, l)
      _       -> fail "Error parsing string to expand"


token1ToToken2 :: [Token1] -> [Token2]
token1ToToken2 [] = []
token1ToToken2 toks = 
    let (chars, rest) = splitAtPred toks (\t -> case t of
                                                  ShowVar1 _ -> True
                                                  StrVar1 _  -> True
                                                  _          -> False)
        (vars, rest') = splitAtPred rest (\t -> case t of
                                                  LitChar _ -> True
                                                  _         -> False)
        s = map (\t -> case t of LitChar c -> c; _ -> error "token1ToToken2:s") chars
        vars' = map (\t -> case t of
                             ShowVar1 v -> ShowVar2 v
                             StrVar1 v  -> StrVar2 v
                             _          -> error "token1ToToken2") vars
        in (if s /= "" then [LitStr s] else []) ++ vars' ++ 
           token1ToToken2 rest'


expand :: String -> ExpQ
expand s =
    do tokens <- parse s
       --runIO $ putStrLn ("parsing ok: " ++ show tokens)
       let tokens2 = token1ToToken2 tokens
       --runIO $ putStrLn ("tokens2: " ++ show tokens2)
       exps <- sequence $ map toExp tokens2
       conc <- [| (++) |]
       return $ foldr 
                  (\l r -> InfixE (Just l) conc (Just r))
                  (LitE (StringL ""))
                  exps
    where toExp (LitStr s')   = litE (StringL s')
          toExp (ShowVar2 s') = [| show $(varE (mkName s')) |]
          toExp (StrVar2 s')  = varE (mkName s')


-- Local Variables: **
-- indent-tabs-mode: nil **
-- tab-width: 4 **
-- End: **
