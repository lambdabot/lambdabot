-- 	$Id: ChessModule.hs,v 1.5 2003/05/03 19:13:26 mlang Exp $	

module ChessModule where

import IRC
import Data.FiniteMap
import Control.Monad.State
import Data.IORef
import Data.Dynamic
import Util
import Board
import ABEval(bestMove)
import Move(applyMove, legalMovesFor, shortAlg)

newtype ChessModule = ChessModule ()

chessModule = ChessModule ()

data ChessState = ChessState (FiniteMap String (Colour,Board))

initialChessState = ChessState emptyFM

instance Module ChessModule where
    moduleName   m = return "chess"
    commands     m = return ["ply", "reset-chess","validmoves","board","flip-board"]
    moduleInit   m =
        do s <- get
           newRef <- liftIO . newIORef $ ModuleState (initialChessState)
           let stateMap = ircModuleState s
           put (s { ircModuleState =
                    addToFM stateMap "chess" newRef })
    process      m msg target cmd rest =
        do let nick = head $ words rest
           maybeChessRef <- gets (\s -> lookupFM (ircModuleState s) "chess")
           case maybeChessRef of
             Just chessRef ->
               do chessFMState <- liftIO $ readIORef chessRef
		  let (ChessState fm) = stripMS chessFMState
                  case (lookupFM fm target) of
                      Nothing -> do resetChess chessRef target
                                    processCommand chessRef msg target cmd rest
                      Just _  -> processCommand chessRef msg target cmd rest
             Nothing ->
               do moduleInit m
		  process m msg target cmd rest

resetChess chessRef target
    = do chessFMState <- liftIO $ readIORef chessRef
         let (ChessState fm::ChessState) = stripMS chessFMState
         liftIO $ writeIORef chessRef $
           ModuleState $ ChessState $ addToFM fm target (White,initialBoard)

processCommand chessRef msg target cmd rest
  = do chessState <- liftIO $ readIORef chessRef
       let (ChessState fm::ChessState) = stripMS chessState
       let (Just (c::Colour,currentBoard::Board)) = lookupFM fm target
       case cmd of
	 "ply"       ->
             if (length rest) == 0 
                then ircPrivmsg target "must give a valid move" 
                else
                  do
                    let bmove = badmove
                        gmove = goodmove chessRef c currentBoard target
                        mbmove = shortAlg rest c currentBoard 
                    res <- case mbmove of
                               Right mv -> liftIO $ gmove mv
                               Left _ -> return bmove
		    ircPrivmsg target res
	 "validmoves" ->
             ircPrivmsg target (show $ legalMovesFor White currentBoard)
         "board"      ->
             ircPrivmsg target $ show currentBoard
         "flip-board" ->
	     do chessFMState <- liftIO $ readIORef chessRef
		let (ChessState fm) = stripMS chessFMState
		liftIO $ writeIORef chessRef $ ModuleState $
		        ChessState (addToFM fm target ((opponent c),currentBoard))
  		res <- maybe
		    (return "I can not move here")
		    (liftIO . opponentResponse currentBoard chessRef (opponent c) target)
		    (bestMove c 4 currentBoard)
                ircPrivmsg target res
         "reset-chess"    ->
             resetChess chessRef target

--badmove :: [Char]
badmove = "not a valid move"

--goodmove :: a -> Board -> b -> String
goodmove chessRef c currentBoard target m = 
    let bd2 = applyMove m currentBoard in
    maybe
    (return "I can not move here")
    (opponentResponse bd2 chessRef c target)
    (bestMove (opponent c) 4 bd2)
    
opponentResponse boardState chessRef c target m =
    do chessFMState <- liftIO $ readIORef chessRef
       let (ChessState fm) = stripMS chessFMState
       writeIORef chessRef (ModuleState (ChessState (addToFM fm target (c,(applyMove m boardState)))))
       return ("I move: " ++ (show m))
                            


-- lots of Typeable instances so we can stuff the board into an IORef
boardTyCon = mkTyCon "Board"

instance Typeable Board where
    typeOf _ =
        mkAppTy boardTyCon [typeOf (undefined :: [(Kind,Square)]),
                            typeOf (undefined :: [(Kind,Square)]),
                            typeOf (undefined :: [(Colour,Castle)]) ]

kindTyCon = mkTyCon "Kind"

instance Typeable Kind where
    typeOf _ = 
        mkAppTy kindTyCon []

colourTyCon = mkTyCon "Colour"

instance Typeable Colour where
    typeOf _ = 
        mkAppTy colourTyCon []

squareTyCon = mkTyCon "Square"

instance Typeable Square where
    typeOf _ = 
        mkAppTy squareTyCon [typeOf (undefined :: Int),typeOf (undefined :: Int)]

castleTyCon = mkTyCon "Castle"

instance Typeable Castle where
    typeOf _ = 
        mkAppTy castleTyCon []


chessStateTyCon = mkTyCon "ChessState"

instance Typeable ChessState where
  typeOf _ = mkAppTy chessStateTyCon [typeOf (undefined :: (FiniteMap String (Colour,Board)))]
