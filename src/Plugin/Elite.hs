{-# LANGUAGE TemplateHaskell #-}
-- (c) Josef Svenningsson, 2005
-- Licence: No licence, public domain

-- Inspired by the following page:
-- http://www.microsoft.com/athome/security/children/kidtalk.mspx
module Plugin.Elite (theModule) where

import Plugin
import qualified Text.Regex as R

import Control.Arrow

plugin "Elite"

instance Module EliteModule where
    moduleCmds = return
        [ (command "elite")
            { aliases = ["leet", "l33t", "1337"]
            , help = say "elite <phrase>. Translate English to elitespeak"
            , process = \args -> case words args of
                 [] -> say "Say again?"
                 wds -> do let instr = map toLower (unwords wds)
                           say =<< io (translateLine instr)
                           
            }
        ]

translateLine = fmap (dropWhile isSpace) . translate . (' ':)
-- extra space allows whole-word patterns to match at start

translate :: String -> IO String
translate []  = return []
translate str@(hd:tl) = do
    let alts = [ (subst match,rest)
               | (re, subst) <- ruleList
               , ([], match, rest, _) <- maybeToList (R.matchRegexAll re str)
               ]
    (subst,rest) <- stdGetRandItem alts
    liftM (subst ++) (translate rest)

ruleList :: [(Regex, String -> String)]
ruleList = map (first regex')
    [ (".",     id            )
    , (".",     map toUpper   )
    , ("a",     const "4"     )
    , ("b",     const "8"     )
    , (" be ",  const " b "   )
    , ("c",     const "("     )
    , ("ck",    const "xx"    )
    , ("cks ",  const "x "    )
    , ("cks ",  const "x0rs " )
    , ("cks ",  const "x0rz " )
    , (" cool ",const " kewl ")
    , ("e",     const "3"     )
    , ("elite", const "1337"  )
    , ("elite", const "leet"  )
    , ("f",     const "ph"    )
    , (" for ", const " 4 "   )
    , ("g",     const "9"     )
    , ("h",     const "|-|"   )
    , ("k",     const "x"     )
    , ("l",     const "|"     )
    , ("l",     const "1"     )
    , ("m",     const "/\\/\\")
    , ("o",     const "0"     )
    , ("ph",    const "f"     )
    , ("s",     const "z"     )
    , ("s",     const "$"     )
    , ("s",     const "5"     )
    , ("s ",    const "z0rz " )
    , ("t",     const "7"     )
    , ("t",     const "+"     )
    , (" the ", const " teh " )
    , (" to ",  const " 2 "   )
    , (" to ",  const " too " )
    , (" to ",  const " tu "  )
    , (" too ", const " to "  )
    , ("v",     const "\\/"   )
    , ("w",     const "\\/\\/")
    , (" you ", const " u "   )
    , (" you ", const " yu "  )
    , (" you ", const " joo " )
    , ("z",     const "s"     )
    ]

