{-

This is an interpreter of the brainf*ck language, written in
the pure, lazy, functional language Haskell.

Copyright (C) 2006 by Jason Dagit <dagit@codersbase.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA -}

module Language.Brainfuck where

import Data.Array.IO
import Data.Array hiding (array)
import Data.Array.Base   (unsafeRead, unsafeWrite, array)
import Data.Word         ( Word8 )
import Data.Char         ( ord, chr )
import Data.List         ( groupBy )
import Data.Maybe        ( catMaybes )
import Control.Monad.State

{- | The complete BF language:

* \>    Increment the pointer.
* \<    Decrement the pointer.
* +     Increment the byte at the pointer.
* \-    Decrement the byte at the pointer.
* .     Output the byte at the pointer.
* ,     Input a byte and store it in the byte at the pointer.
* [     Jump forward past the matching ] if the byte at the pointer is zero.
* ]     Jump backward to the matching [ unless the byte at the pointer is zero.

-}

data Command = IncPtr
             | IncPtrBy !Int  -- ^ Increment pointer by set amount
             | DecPtr
             | IncByte
             | IncByteBy !Int -- ^ Increment by a set amount
             | DecByte
             | OutputByte
         --  | InputByte
             | JmpForward  !Int -- ^ nesting level
             | JmpBackward !Int -- ^ nesting level
             | SetIpTo !Int   -- ^ Sets the instruction ptr to a specific value
             | Halt
             | Ignored
             deriving (Show, Eq)

type Core = IOUArray Int Word8

type InstPtr = Int
type CorePtr = Int
data BF = BF !Core !CorePtr !InstPtr

instance Show BF where
    show (BF _ cp ip) = "BF <core> CorePtr = " ++ show cp ++ " InstPtr = " ++ show ip

coreSize = 30000

core :: IO Core
core = newArray (0, coreSize - 1) (0::Word8)

decode :: Char -> State Int Command
decode '>' = return IncPtr
decode '<' = return DecPtr
decode '+' = return IncByte
decode '-' = return DecByte
decode '.' = return OutputByte
-- decode ',' = return InputByte
decode '[' = do n <- get
                put (n+1)
                return $ JmpForward n
decode ']' = do n <- get
                put (n-1)
                return $ JmpBackward (n-1)
decode '@' = return Halt
decode _   = return Ignored

debug :: Bool
debug = False

incIP :: InstPtr -> InstPtr
incIP = (+ 1)
{-# INLINE incIP #-}

incCP :: CorePtr -> CorePtr
incCP = (`mod` coreSize) . (1 +)
{-# INLINE incCP #-}

decCP :: CorePtr -> CorePtr
decCP = (`mod` coreSize) . subtract 1
{-# INLINE decCP #-}

doCommand :: Array Int Command -> BF -> IO BF
doCommand cmds bf@(BF _ _ ip) = doCommand' (cmds ! ip) cmds bf
  where
  doCommand' :: Command -> Array Int Command -> BF -> IO BF
  doCommand' Halt _ _ = undefined
  doCommand' Ignored _ (BF c cp ip) = {-# SCC "Ignored" #-} do
    when debug $ putStrLn $ "Ignored " ++ show bf
    return (BF c cp (incIP ip))
  doCommand' IncPtr _ bf@(BF c cp ip) = {-# SCC "IncPtr" #-} do
    when debug $ putStrLn $ "IncPtr " ++ show bf
    return (BF c (incCP cp) (incIP ip))
  doCommand' DecPtr _ bf@(BF c cp ip) = {-# SCC "DecPtr" #-} do
    when debug $ putStrLn $ "DecPtr " ++ show bf
    return (BF c (decCP cp) (incIP ip))
  doCommand' (IncPtrBy n) _ bf@(BF c cp ip) = {-# SCC "IncPtrBy" #-} do
    when debug $ putStrLn $ "IncPtrBy " ++ show n ++ " " ++ show bf
    return (BF c ((cp + n) `mod` coreSize) (incIP ip))
  doCommand' IncByte _ bf = {-# SCC "IncByte" #-} do
    when debug $ putStrLn $ "IncByte " ++ show bf
    updateByte bf (+1)
  doCommand' DecByte _ bf = {-# SCC "DecByte" #-} do
    when debug $ putStrLn $ "DecByte " ++ show bf
    updateByte bf (subtract 1)
  doCommand' (IncByteBy n) _ bf = {-# SCC "IncByteBy" #-} do
    when debug $ putStrLn $ "IncByteBy " ++ show n ++ " " ++ show bf
    updateByte bf (+ fromIntegral n)
  doCommand' OutputByte _ bf@(BF c cp ip) = {-# SCC "OutputByte" #-} do
    when debug $ putStrLn $ "OutputByte " ++ show bf
    c' <- unsafeRead c cp
    putChar (word8ToChr c')
    return (BF c cp (incIP ip))

{-
  doCommand' InputByte _ bf@(BF c cp ip) = {-# SCC "InputByte" #-} do
    when debug $ putStrLn $ "InputByte " ++ show bf
    c' <- getChar
    let newByte = chrToWord8 c'
    unsafeWrite c cp newByte
    return (BF c cp (incIP ip))
-}

  doCommand' (JmpForward n) cmds bf@(BF c cp ip) = {-# SCC "JmpForw" #-} do
    c' <- unsafeRead c cp
    case c' of
      0 -> {-# SCC "JmpForward1" #-} do
        when debug $ putStrLn $ "JmpForward1 " ++ show bf
        return (BF c cp newInstPtr)
      _ -> {-# SCC "JmpForward2" #-} do
        when debug $ putStrLn $ "JmpForward2 " ++ show bf
        let newBF = (BF c cp (incIP ip))
        when debug $ putStrLn $ "JmpForward3" ++ show newBF
        return newBF
    where
    -- we add one to go one past the next back jump
    newInstPtr = (nextJmp cmds ip (+1) (JmpBackward n)) + 1
  doCommand' (JmpBackward n) cmds bf@(BF c cp ip) = {-# SCC "JmpBack" #-} do
    c' <- unsafeRead c cp
    if (c' /= 0)
      then do when debug $ putStrLn $ "JmpBackward1 " ++ show bf
              return (BF c cp newInstPtr)
      else do when debug $ putStrLn $ "JmpBackward2 " ++ show bf
              return (BF c cp (incIP ip))
    where
    newInstPtr = nextJmp cmds ip (subtract 1) (JmpForward n)
  doCommand' (SetIpTo i) _ bf@(BF c cp ip) = {-# SCC "SetIPTo" #-} do
    c' <- unsafeRead c cp
    when debug $ putStrLn $ "SetIpTo " ++ show i ++ " "
                          ++ show bf ++ " @" ++ show c'
    -- jmping behaves differently depending on jmp forward vs. backward
    -- we handle that with pos. vs. neg addresses
    -- Note: SetIpTo 0 is always a JmpBackward
    -- Because the first instruction cannot be SetIpTo 0
    if i > 0
      then if (c' == 0)
             then return $ BF c cp i
             else return $ BF c cp (incIP ip)
      else if (c' /= 0)
             then return $ BF c cp (-i)
             else return $ BF c cp (incIP ip)

nextJmp :: Array Int Command
        -> InstPtr
        -> (InstPtr -> InstPtr) -> Command -> InstPtr
nextJmp cmds ip f cmd = if cmds ! ip == cmd
                          then ip
                          else nextJmp cmds (f ip) f cmd

chrToWord8 :: Char -> Word8
chrToWord8 = fromIntegral . ord

word8ToChr :: Word8 -> Char
word8ToChr = chr . fromIntegral

updateByte (BF c cp ip) f = do
  e  <- unsafeRead c cp
  unsafeWrite c cp (f e)
  return (BF c cp (incIP ip))
{-# INLINE updateByte #-}

loadProgram :: String -> Array Int Command
loadProgram [] = array (0, 0) [(0, Halt)]

-- adding a halt on to the end fixes a bug when called from an irc session
loadProgram prog = optimize (cs++[Halt])
  where
  cs = fst $ runState (mapM decode prog) 0
  n  = length cs -- strictness

optimize :: [Command] -> Array Int Command
optimize cmds = listArray (0, (length reduced)-1) reduced
  where
  reduced = phase3 . phase2 . phase1 $ cmds
  -- phase1 removes ignored things
  phase1 :: [Command] -> [Command]
  phase1 = filter (/=Ignored)
  -- in phase2 group inc/dec into special instructions
  phase2 :: [Command] -> [Command]
  phase2 cs = concat $ map reduce $ groupBy (==) cs
    where
    reduce :: [Command] -> [Command]
    reduce cs
      | all (==IncPtr)  cs = [IncPtrBy  (length cs)]
      | all (==DecPtr)  cs = [IncPtrBy  (-(length cs))]
      | all (==IncByte) cs = [IncByteBy (length cs)]
      | all (==DecByte) cs = [IncByteBy (-(length cs))]
      | otherwise          = cs
  -- now we can turn jumps into changes of the ip
  phase3 :: [Command] -> [Command]
  phase3 cmds = updates (updates cmds jmpBs) jmpFs
    where
    jmpBs = calcJmpBs (zip [0..] cmds)
    jmpFs = calcJmpFs (zip [0..] cmds)
    update :: [a] -> (Int, a) -> [a]
    update xs (i, a) = take i xs ++ [a] ++ drop (i+1) xs
    updates :: [a] -> [(Int, a)] -> [a]
    updates xs []     = xs
    updates xs (u:us) = updates (update xs u) us
    nested :: Command -> Int
    nested (JmpForward  n) = n
    nested (JmpBackward n) = n
    nested _               = undefined
    isJmpB (JmpBackward _) = True
    isJmpB _               = False
    isJmpF (JmpForward  _) = True
    isJmpF _               = False
    calcJmpBs :: [(Int, Command)] -> [(Int, Command)]
    calcJmpBs cmds = catMaybes $ map newCmd (filter (isJmpB . snd) cmds)
      where
      newCmd (i, c) = absJmpB (i, findPrevJmpF (map snd cmds) i (nested c))
    calcJmpFs :: [(Int, Command)] -> [(Int, Command)]
    calcJmpFs cmds = catMaybes $ map newCmd (filter (isJmpF . snd) cmds)
      where
      newCmd (i, c) = absJmpF (i, findNextJmpB (map snd cmds) i (nested c))
    absJmpB :: (Int, Maybe Int) -> Maybe (Int, Command)
    absJmpB (_, Nothing) = Nothing
    absJmpB (i, Just n)  = Just $ (i, SetIpTo (-n))
    absJmpF (_, Nothing) = Nothing
    absJmpF (i, Just n)  = Just $ (i, SetIpTo (n+1))
    findPrevJmpF :: [Command]
                 -> Int -- ^ index to start at
                 -> Int -- ^ nesting level to match
                 -> Maybe Int -- ^ index of next JmpF
    findPrevJmpF _    i _ | i < 0 = Nothing
    findPrevJmpF cmds i n = case (cmds !! i) of
                              (JmpForward l) | l == n -> Just i
                              _ -> findPrevJmpF cmds (i-1) n

    findNextJmpB :: [Command]
                 -> Int -- ^ index to start at
                 -> Int -- ^ nesting level to match
                 -> Maybe Int -- ^ index of next JmpF
    findNextJmpB cmds i _ | i >= length cmds = Nothing
    findNextJmpB cmds i    n = case (cmds !! i) of
                                 (JmpBackward l) | l == n -> Just i
                                 _ -> findNextJmpB cmds (i+1) n

execute :: Array Int Command -> Int -> BF -> IO ()
execute cmds n bf@(BF _ _ ip) = do
  if ip >= n || cmds ! ip == Halt
    then halt
    else doCommand cmds bf >>= execute cmds n

halt = if debug
         then putStrLn "Machine Halted.\n"
         else putStrLn "\n"