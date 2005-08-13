module IRC where

import DeepSeq
import Util (split, breakOnGlue, clean)
import qualified Util (concatWith) -- TODO: rename the damn thing.

data Message
  = Message {
        msgPrefix   :: String,
        msgCommand  :: String,
        msgParams   :: [String]
  }
  deriving (Show)

instance DeepSeq Message where
  deepSeq m
    = deepSeq (msgPrefix m) . deepSeq (msgCommand m) . deepSeq (msgParams m)

mkMessage :: String -> [String] -> Message
mkMessage cmd params
  = Message { msgPrefix = "", msgCommand = cmd, msgParams = params }

nick     :: Message -> String
nick msg = fst $ breakOnGlue "!" (msgPrefix msg)

-- | 'channels' converts an Message to a list of channels.
channels :: Message -> [String]
channels msg
  = let cstr = head $ msgParams msg
    in map (\(x:xs) -> if x == ':' then xs else x:xs) (split "," cstr)
           -- solves what seems to be an inconsistency in the parser

{-
ircuser     :: Message -> String
ircuser msg = head $ split "@" $ (split "!" (msgPrefix msg)) !! 1

irchost     :: Message -> String
irchost msg = (split "@" (msgPrefix msg)) !! 1
-}

privmsg :: String -> String -> Message
privmsg who msg = mkMessage "PRIVMSG" [who, ':' : clean_msg]
    where clean_msg = case concatMap clean msg of
              str@('@':_) -> ' ':str
              str         -> str


setTopic :: String -> String -> Message
setTopic chan topic = mkMessage "TOPIC" [chan, ':' : topic]

getTopic :: String -> Message
getTopic chan = mkMessage "TOPIC" [chan]

quit :: String -> Message
quit msg = mkMessage "QUIT" [':' : msg]

join :: String -> Message
join loc = mkMessage "JOIN" [loc]

part :: String -> Message
part loc = mkMessage "PART" [loc]

names :: [String] -> Message
names chans = mkMessage "NAMES" [Util.concatWith "," chans]

{-
ircRead :: IRC Message
ircRead = do 
    chanr <- asks ircReadChan
    liftIO (readChan chanr)

ircWrite :: Message -> IRC ()
ircWrite line = do  
    chanw <- asks ircWriteChan
    -- use DeepSeq's $!! to ensure that any Haskell errors in line
    -- are caught now, rather than later on in the other thread
    liftIO (writeChan chanw $!! line)
-}
----------------------------------------------------------------------

encodeMessage :: Message -> String -> String
encodeMessage msg
  = encodePrefix (msgPrefix msg) . encodeCommand (msgCommand msg)
          . encodeParams (msgParams msg)
  where
    encodePrefix [] = id
    encodePrefix prefix = showChar ':' . showString prefix . showChar ' '

    encodeCommand cmd = showString cmd

    encodeParams [] = id
    encodeParams (p:ps) = showChar ' ' . showString p . encodeParams ps


decodeMessage :: String -> Message
decodeMessage line
  = let (prefix, rest1) = decodePrefix (,) line in
    let (cmd, rest2)    = decodeCmd (,) rest1 in
    let params          = decodeParams rest2 in
    Message { msgPrefix = prefix, msgCommand = cmd, msgParams = params }
  where
    decodePrefix k (':':cs)
      = decodePrefix' k cs
      where
        decodePrefix' j ""       = j "" ""
        decodePrefix' j (' ':ds) = j "" ds
        decodePrefix' j (c:ds)   = decodePrefix' (\xs ys -> j (c:xs) ys) ds

    decodePrefix k cs
      = k "" cs

    decodeCmd k []
      = k "" ""
    decodeCmd k (' ':cs)
      = k "" cs
    decodeCmd k (c:cs)
      = decodeCmd (\xs ys -> k (c:xs) ys) cs

    decodeParams :: String -> [String]
    decodeParams xs
      = decodeParams' [] [] xs
      where
        decodeParams' param params []
          | null param = reverse params
          | otherwise  = reverse (reverse param : params)
        decodeParams' param params (' ' : cs)
          | null param = decodeParams' [] params cs
          | otherwise  = decodeParams' [] (reverse param : params) cs
        decodeParams' param params rest@(c@':' : cs)
          | null param = reverse (rest : params)
          | otherwise  = decodeParams' (c:param) params cs
        decodeParams' param params (c:cs)
          = decodeParams' (c:param) params cs

{-
getFirstWord :: String -> String
getFirstWord line = takeWhile (/=' ') line
-}

{-
lowQuote :: String -> String
lowQuote [] = []
lowQuote ('\0':cs)   = '\020':'0'    : lowQuote cs
lowQuote ('\n':cs)   = '\020':'n'    : lowQuote cs
lowQuote ('\r':cs)   = '\020':'r'    : lowQuote cs
lowQuote ('\020':cs) = '\020':'\020' : lowQuote cs
lowQuote (c:cs)      = c : lowQuote cs

lowDequote :: String -> String
lowDequote [] = []
lowDequote ('\020':'0'   :cs) = '\0'   : lowDequote cs
lowDequote ('\020':'n'   :cs) = '\n'   : lowDequote cs
lowDequote ('\020':'r'   :cs) = '\r'   : lowDequote cs
lowDequote ('\020':'\020':cs) = '\020' : lowDequote cs
lowDequote ('\020'       :cs) = lowDequote cs
lowDequote (c:cs)             = c : lowDequote cs

ctcpQuote :: String -> String
ctcpQuote [] = []
ctcpQuote ('\001':cs) = '\134':'a'    : ctcpQuote cs
ctcpQuote ('\134':cs) = '\134':'\134' : ctcpQuote cs
ctcpQuote (c:cs)      = c : ctcpQuote cs

ctcpDequote :: String -> String
ctcpDequote [] = []
ctcpDequote ('\134':'a'   :cs) = '\001' : ctcpDequote cs
ctcpDequote ('\134':'\134':cs) = '\134' : ctcpDequote cs
ctcpDequote ('\134':cs)        = ctcpDequote cs
ctcpDequote (c:cs)             = c : ctcpDequote cs
-}

