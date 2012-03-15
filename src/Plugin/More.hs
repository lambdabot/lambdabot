{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | Support for more(1) buffering
module Plugin.More (theModule) where

import Plugin

import Lambdabot.Message( Nick )

plugin "More"

type MoreState = GlobalPrivate () [String]

-- the @more state is handled centrally
instance Module MoreModule where
    type ModuleState MoreModule = MoreState
    
    moduleCmds _ = 
        [ (command "more")
            { help = say "@more. Return more output from the bot buffer."
            , process = \_ -> do
                target <- getTarget
                morestate <- readPS target
                -- TODO: test theory that we can just "say" morestate; 
                --       it should end up going through the moreFilter as needed
                case morestate of
                    Nothing -> return ()
                    Just ls -> lift (moreFilter target ls)
                        >>= mapM_ (lb . ircPrivmsg' target)
            }
        ]
    moduleDefState _            = return $ mkGlobalPrivate 20 ()
    moduleInit   _              = bindModule2 moreFilter >>=
                                      ircInstallOutputFilter

moreFilter :: Nick -> [String] -> More [String]
moreFilter target msglines = do
    let (morelines, thislines) = case drop (maxLines+2) msglines of
          [] -> ([],msglines)
          _  -> (drop maxLines msglines, take maxLines msglines)
    writePS target $ if null morelines then Nothing else Just morelines
    return $ thislines ++ if null morelines
                          then []
                          else ['[':shows (length morelines) " @more lines]"]

    where maxLines = 5 -- arbitrary, really
