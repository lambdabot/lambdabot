{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | Support for more(1) buffering
module Plugin.More (theModule) where

import Plugin

import Lambdabot.Message( Nick )

$(plugin "More")

type MoreState = GlobalPrivate () [String]

-- the @more state is handled centrally
instance Module MoreModule where
    type ModuleState MoreModule = MoreState
    
    moduleCmds _ = 
        [ (command "more")
            { help = say "@more. Return more output from the bot buffer."
            , process = \_ -> do
                target <- getTarget
                morestate <- lift (readPS target)
                lift $ case morestate of
                    Nothing -> return ()
                    Just ls -> mapM_ (lift . ircPrivmsg' target) =<< moreFilter target ls
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
