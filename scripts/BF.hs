-- This is an interpreter of the braif*ck language, written in
-- the pure, lazy, functional language Haskell.
-- 
-- Copyright (C) 2006 by Jason Dagit <dagit@codersbase.com>
--                                                                           
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--                                                                           
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--                                                                           
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

import Array
import Data.Word ( Word8(..) )
import Data.Char ( ord, chr )
import Foreign ( unsafePerformIO )
import Monad ( when )
import System.Posix.Resource

rlimit = ResourceLimit 3

main = setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit) >> run

-- run prog = execute (loadProgram prog) (BF core 0 0)
run = do
  prog <- getContents
  execute (loadProgram prog) (BF core 0 0)

{- | The complete BF language:

* \> 	Increment the pointer.
* \< 	Decrement the pointer.
* + 	Increment the byte at the pointer.
* \- 	Decrement the byte at the pointer.
* . 	Output the byte at the pointer.
* , 	Input a byte and store it in the byte at the pointer.
* [ 	Jump forward past the matching ] if the byte at the pointer is zero.
* ] 	Jump backward to the matching [ unless the byte at the pointer is zero.

-}

data Command = IncPtr
             | DecPtr
             | IncByte
             | DecByte
             | OutputByte
             | InputByte
             | JmpForward
             | JmpBackward 
             | Halt
             | Ignored
             deriving (Show, Eq)

type Core = Array Int Word8

type InstPtr = Int
type CorePtr = Int
data BF = BF Core CorePtr InstPtr

instance Show BF where
    show (BF c cp ip) = "BF <core> CorePtr = " ++ show cp ++ " InstPtr = " ++ show ip

coreSize = 30000

core :: Core
core = array (0, coreSize - 1) [(i, 0) | i <- [0..coreSize - 1]]
decode :: Char -> Command
decode '>' = IncPtr
decode '<' = DecPtr
decode '+' = IncByte
decode '-' = DecByte
decode '.' = OutputByte
decode ',' = InputByte
decode '[' = JmpForward
decode ']' = JmpBackward
decode '@' = Halt
decode _   = Ignored

debug = False

incIP :: InstPtr -> InstPtr
incIP ip = ip + 1

incCP :: CorePtr -> CorePtr
incCP cp = (cp + 1) `mod` coreSize

decCP :: CorePtr -> CorePtr
decCP cp = (cp - 1) `mod` coreSize
 
doCommand :: [Command] -> BF -> IO BF
doCommand cmds bf@(BF _ _ ip) = doCommand' (cmds !! ip) cmds bf
  where
  doCommand' :: Command -> [Command] -> BF -> IO BF
  doCommand' Halt _ _ = undefined
  doCommand' Ignored _ (BF c cp ip) = do
    when debug $ putStrLn $ "Ignored " ++ show bf
    return (BF c cp (incIP ip))
  doCommand' IncPtr _ bf@(BF c cp ip) = do
    when debug $ putStrLn $ "IncPtr " ++ show bf
    return (BF c (incCP cp) (incIP ip))
  doCommand' DecPtr _ bf@(BF c cp ip) = do
    when debug $ putStrLn $ "DecPtr " ++ show bf
    return (BF c (decCP cp) (incIP ip))
  doCommand' IncByte _ bf = do
    when debug $ putStrLn $ "IncByte " ++ show bf
    return (updateByte bf (+1))
  doCommand' DecByte _ bf = do
    when debug $ putStrLn $ "DecByte " ++ show bf
    return (updateByte bf (subtract 1))
  doCommand' OutputByte _ bf@(BF c cp ip) = do 
    when debug $ putStrLn $ "OutputByte " ++ show bf
    let c' = word8ToChr (c ! cp)
    putChar c'
    return (BF c cp (incIP ip))
  doCommand' InputByte _ bf@(BF c cp ip) = do
    when debug $ putStrLn $ "InputByte " ++ show bf
    c' <- getChar
    let newByte = chrToWord8 c'
    let newCore = c//[(cp, newByte)]
    return (BF newCore cp (incIP ip))
  doCommand' JmpForward cmds bf@(BF c cp ip) 
    | (c ! cp) == 0 = do 
      when debug $ putStrLn $ "JmpForward1 " ++ show bf
      return (BF c cp newInstPtr)
    | otherwise = do
      when debug $ putStrLn $ "JmpForward2 " ++ show bf
      let newBF = (BF c cp (incIP ip))
      when debug $ putStrLn $ "JmpForward3" ++ show newBF
      return newBF
    where
    -- we add one to go one past the next back jump
    newInstPtr = (nextJmp cmds ip (+1) JmpBackward) + 1 
  doCommand' JmpBackward cmds bf@(BF c cp ip)
    | (c ! cp) /= 0 = do
      when debug $ putStrLn $ "JmpBackward1 " ++ show bf
      return (BF c cp newInstPtr)
    | otherwise = do
      when debug $ putStrLn $ "JmpBackward2 " ++ show bf
      return (BF c cp (incIP ip))
    where
    newInstPtr = nextJmp cmds ip (subtract 1) JmpForward

nextJmp :: [Command] -> InstPtr -> (InstPtr -> InstPtr) -> Command -> InstPtr
nextJmp cmds ip f cmd = if cmds !! ip == cmd
                          then ip
                          else nextJmp cmds (f ip) f cmd

chrToWord8 :: Char -> Word8
chrToWord8 = fromIntegral . ord

word8ToChr :: Word8 -> Char
word8ToChr = chr . fromIntegral

updateByte (BF c cp ip) f = BF newCore cp (incIP ip)
  where
  newByte :: Word8
  newByte = f (c ! cp)
  newCore :: Core
  newCore = c//[(cp, newByte)]

loadProgram :: String -> [Command]
loadProgram prog = map decode prog

execute :: [Command] -> BF -> IO ()
execute [] bf = halt
execute cmds bf@(BF c cp ip) = do
  if ip == (length cmds)
    then halt
    else do
      let nextCmd = cmds !! ip
      if nextCmd == Halt
        then halt
        else do
          newState <- doCommand cmds bf
          execute cmds newState

halt = putStrLn "Machine Halted."

-- Example Programs

helloWorld = 
  ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]"++
  "<.#>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++["++
  "<++++>-]<+.[-]++++++++++."


-- this one is not working at the moment...
bottles = 
  "99 Bottles of Beer in Urban Mueller's BrainF*** (The actual"++
  "name is impolite)"++
  ""++
  "by Ben Olmstead"++
  ""++
  "ANSI C interpreter available on the internet; due to"++
  "constraints in comments the address below needs to have the"++
  "stuff in parenthesis replaced with the appropriate symbol:"++
  ""++
  "http://www(dot)cats(dash)eye(dot)com/cet/soft/lang/bf/"++
  ""++
  "Believe it or not this language is indeed Turing complete!"++
  "Combines the speed of BASIC with the ease of INTERCAL and"++
  "the readability of an IOCCC entry!"++
  ""++
  ">+++++++++[<+++++++++++>-]<[>[-]>[-]<<[>+>+<<-]>>[<<+>>-]>>>"++
  "[-]<<<+++++++++<[>>>+<<[>+>[-]<<-]>[<+>-]>[<<++++++++++>>>+<"++
  "-]<<-<-]+++++++++>[<->-]>>+>[<[-]<<+>>>-]>[-]+<<[>+>-<<-]<<<"++
  "[>>+>+<<<-]>>>[<<<+>>>-]>[<+>-]<<-[>[-]<[-]]>>+<[>[-]<-]<+++"++
  "+++++[<++++++<++++++>>-]>>>[>+>+<<-]>>[<<+>>-]<[<<<<<.>>>>>-"++
  "]<<<<<<.>>[-]>[-]++++[<++++++++>-]<.>++++[<++++++++>-]<++.>+"++
  "++++[<+++++++++>-]<.><+++++..--------.-------.>>[>>+>+<<<-]>"++
  ">>[<<<+>>>-]<[<<<<++++++++++++++.>>>>-]<<<<[-]>++++[<+++++++"++
  "+>-]<.>+++++++++[<+++++++++>-]<--.---------.>+++++++[<------"++
  "---->-]<.>++++++[<+++++++++++>-]<.+++..+++++++++++++.>++++++"++
  "++[<---------->-]<--.>+++++++++[<+++++++++>-]<--.-.>++++++++"++
  "[<---------->-]<++.>++++++++[<++++++++++>-]<++++.-----------"++
  "-.---.>+++++++[<---------->-]<+.>++++++++[<+++++++++++>-]<-."++
  ">++[<----------->-]<.+++++++++++..>+++++++++[<---------->-]<"++
  "-----.---.>>>[>+>+<<-]>>[<<+>>-]<[<<<<<.>>>>>-]<<<<<<.>>>+++"++
  "+[<++++++>-]<--.>++++[<++++++++>-]<++.>+++++[<+++++++++>-]<."++
  "><+++++..--------.-------.>>[>>+>+<<<-]>>>[<<<+>>>-]<[<<<<++"++
  "++++++++++++.>>>>-]<<<<[-]>++++[<++++++++>-]<.>+++++++++[<++"++
  "+++++++>-]<--.---------.>+++++++[<---------->-]<.>++++++[<++"++
  "+++++++++>-]<.+++..+++++++++++++.>++++++++++[<---------->-]<"++
  "-.---.>+++++++[<++++++++++>-]<++++.+++++++++++++.++++++++++."++
  "------.>+++++++[<---------->-]<+.>++++++++[<++++++++++>-]<-."++
  "-.---------.>+++++++[<---------->-]<+.>+++++++[<++++++++++>-"++
  "]<--.+++++++++++.++++++++.---------.>++++++++[<---------->-]"++
  "<++.>+++++[<+++++++++++++>-]<.+++++++++++++.----------.>++++"++
  "+++[<---------->-]<++.>++++++++[<++++++++++>-]<.>+++[<----->"++
  "-]<.>+++[<++++++>-]<..>+++++++++[<--------->-]<--.>+++++++[<"++
  "++++++++++>-]<+++.+++++++++++.>++++++++[<----------->-]<++++"++
  ".>+++++[<+++++++++++++>-]<.>+++[<++++++>-]<-.---.++++++.----"++
  "---.----------.>++++++++[<----------->-]<+.---.[-]<<<->[-]>["++
  "-]<<[>+>+<<-]>>[<<+>>-]>>>[-]<<<+++++++++<[>>>+<<[>+>[-]<<-]"++
  ">[<+>-]>[<<++++++++++>>>+<-]<<-<-]+++++++++>[<->-]>>+>[<[-]<"++
  "<+>>>-]>[-]+<<[>+>-<<-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<>>[<+>-]<"++
  "<-[>[-]<[-]]>>+<[>[-]<-]<++++++++[<++++++<++++++>>-]>>>[>+>+"++
  "<<-]>>[<<+>>-]<[<<<<<.>>>>>-]<<<<<<.>>[-]>[-]++++[<++++++++>"++
  "-]<.>++++[<++++++++>-]<++.>+++++[<+++++++++>-]<.><+++++..---"++
  "-----.-------.>>[>>+>+<<<-]>>>[<<<+>>>-]<[<<<<++++++++++++++"++
  ".>>>>-]<<<<[-]>++++[<++++++++>-]<.>+++++++++[<+++++++++>-]<-"++
  "-.---------.>+++++++[<---------->-]<.>++++++[<+++++++++++>-]"++
  "<.+++..+++++++++++++.>++++++++[<---------->-]<--.>+++++++++["++
  "<+++++++++>-]<--.-.>++++++++[<---------->-]<++.>++++++++[<++"++
  "++++++++>-]<++++.------------.---.>+++++++[<---------->-]<+."++
  ">++++++++[<+++++++++++>-]<-.>++[<----------->-]<.+++++++++++"++
  "..>+++++++++[<---------->-]<-----.---.+++.---.[-]<<<]"++
  "@"

helloum =
  "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.@"

-- can't figure this one out either...
sort =
  "Here is a Brainf*** program that bubblesorts its input and spits it out:"++
  ">>>>>,+[>>>,+]<<<[<<<"++
  "[>>>[-<<<-<+>[>]>>]<<<[<]>>"++
  "[>>>+<<<-]<[>+>>>+<<<<-]"++
  "<<]>>>[-.[-]]>>>[>>>]<<<]"

toupper =
  ",----------[----------------------.,----------]"