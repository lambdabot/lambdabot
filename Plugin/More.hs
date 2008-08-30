{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Support for more(1) buffering
module Plugin.More (theModule) where

import Plugin

import Message( Nick )

$(plugin "More")

type MoreState = GlobalPrivate () [String]

-- the @more state is handled centrally
instance Module MoreModule MoreState where
    moduleHelp _ _              = "@more. Return more output from the bot buffer."
    moduleCmds   _              = ["more"]
    moduleDefState _            = return $ mkGlobalPrivate 20 ()
    moduleInit   _              = bindModule2 moreFilter >>=
                                      ircInstallOutputFilter
    process      _ _ target _ _ = do
        morestate <- readPS target
        case morestate of
            Nothing -> return []
            Just ls -> do mapM_ (lift . ircPrivmsg' target) =<< moreFilter target ls
                          return []       -- special

moreFilter :: Nick -> [String] -> ModuleLB MoreState
moreFilter target msglines = do
    let (morelines, thislines) = case drop (maxLines+2) msglines of
          [] -> ([],msglines)
          _  -> (drop maxLines msglines, take maxLines msglines)
    writePS target $ if null morelines then Nothing else Just morelines
    return $ thislines ++ if null morelines
                          then []
                          else ['[':shows (length morelines) " @more lines]"]

    where maxLines = 5 -- arbitrary, really
