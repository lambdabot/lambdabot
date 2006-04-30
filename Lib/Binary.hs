{-# OPTIONS -cpp -fasm #-}
--
-- (c) The University of Glasgow 2002
-- (c) Don Stewart 2005-6
--
-- Binary I/O library, with special tweaks for GHC
-- and
-- Unboxed mutable Ints
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     http://www.cs.york.ac.uk/fp/nhc98/

module Lib.Binary ( Binary(..), openBinIO_, putByte, getWord8 ) where

#include "MachDeps.h"

#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

import Data.Char        (ord, chr)
import Foreign          (Int32, Int64, Word8, Word32, Word64
	                    ,Bits(shiftR, shiftL, (.|.), (.&.)))
import System.IO as IO  (Handle, hPutChar, hGetChar)

import GHC.IOBase       (IO(..))
import GHC.Exts

import qualified Data.ByteString as P (hGet,ByteString(..),hPut)

------------------------------------------------------------------------

data BinHandle = BinIO {-# UNPACK #-} !FastMutInt {-# UNPACK #-} !IO.Handle

class Binary a where
    put_   :: BinHandle -> a -> IO ()
    get    :: BinHandle -> IO a

openBinIO_ :: IO.Handle -> IO BinHandle
openBinIO_ h = openBinIO h (error "Binary.BinHandle: no user data")

openBinIO :: Handle -> t -> IO BinHandle
openBinIO h _mod = do
  r <- newFastMutInt
  writeFastMutInt r 0
  return (BinIO r h)

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 (BinIO ix_r h) w = do
    ix <- readFastMutInt ix_r
    hPutChar h (chr (fromIntegral w))	-- XXX not really correct
    writeFastMutInt ix_r (ix+1)
    return ()

getWord8 :: BinHandle -> IO Word8
getWord8 (BinIO ix_r h) = do
    ix <- readFastMutInt ix_r
    c <- hGetChar h
    writeFastMutInt ix_r (ix+1)
    return $! (fromIntegral (ord c))	-- XXX not really correct

getByte :: BinHandle -> IO Word8
getByte = getWord8
{-# INLINE getByte #-}

putByte :: BinHandle -> Word8 -> IO ()
putByte bh w = put_ bh w

-- -----------------------------------------------------------------------------
-- Primitve Word writes

instance Binary Word8 where
  put_ = putWord8
  get  = getWord8

instance Binary Word32 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 24))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|. 
	       (fromIntegral w2 `shiftL` 16) .|. 
	       (fromIntegral w3 `shiftL`  8) .|. 
	       (fromIntegral w4))

instance Binary Word64 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 56))
    putByte h (fromIntegral ((w `shiftR` 48) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 40) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR`  8) .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    w7 <- getWord8 h
    w8 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 56) .|. 
	       (fromIntegral w2 `shiftL` 48) .|. 
	       (fromIntegral w3 `shiftL` 40) .|. 
	       (fromIntegral w4 `shiftL` 32) .|. 
	       (fromIntegral w5 `shiftL` 24) .|. 
	       (fromIntegral w6 `shiftL` 16) .|. 
	       (fromIntegral w7 `shiftL`  8) .|. 
	       (fromIntegral w8))

instance Binary Int32 where
  put_ h w = put_ h (fromIntegral w :: Word32)
  get h    = do w <- get h; return $! (fromIntegral (w::Word32))

instance Binary Int64 where
  put_ h w = put_ h (fromIntegral w :: Word64)
  get h    = do w <- get h; return $! (fromIntegral (w::Word64))

-- -----------------------------------------------------------------------------
-- Instances for standard types

instance Binary Int where
#if SIZEOF_HSINT == 4
    put_ bh i = put_ bh (fromIntegral i :: Int32)
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int32))
#elif SIZEOF_HSINT == 8
    put_ bh i = put_ bh (fromIntegral i :: Int64)
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int64))
#else
#error "unsupported sizeof(HsInt)"
#endif
--    getF bh   = getBitsF bh 32

instance Binary Integer where
    put_ bh (S# i#) = do putByte bh 0; put_ bh (I# i#)
    put_ bh (J# s# a#) = do
 	putByte bh 1;
	put_ bh (I# s#)
	let sz# = sizeofByteArray# a#  -- in *bytes*
	put_ bh (I# sz#)  -- in *bytes*
	putByteArray bh a# sz#
   
    get bh = do 
	b <- getByte bh
	case b of
	  0 -> do (I# i#) <- get bh
		  return (S# i#)
	  _ -> do (I# s#) <- get bh
		  sz <- get bh
		  (BA a#) <- getByteArray bh sz
		  return (J# s# a#)

putByteArray :: BinHandle -> ByteArray# -> Int# -> IO ()
putByteArray bh a s# = loop 0#
  where loop n# 
	   | n# ==# s# = return ()
	   | otherwise = do
	   	putByte bh (indexByteArray a n#)
		loop (n# +# 1#)

getByteArray :: BinHandle -> Int -> IO ByteArray
getByteArray bh (I# sz) = do
  (MBA arr) <- newByteArray sz 
  let loop n
	   | n ==# sz = return ()
	   | otherwise = do
		w <- getByte bh 
		writeByteArray arr n w
		loop (n +# 1#)
  loop 0#
  freezeByteArray arr

data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s', arr #) ->
  (# s', MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s', arr' #) ->
  (# s', BA arr' #) }

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()

writeByteArray arr i w8 = IO $ \s ->
  case fromIntegral w8 of { W# w# -> 
  case writeCharArray# arr i (chr# (word2Int# w#)) s  of { s' ->
  (# s' , () #) }}

indexByteArray :: (Num b) => ByteArray# -> Int# -> b
indexByteArray a# n# = fromIntegral (I# (ord# (indexCharArray# a# n#)))

instance (Binary a, Binary b) => Binary (a,b) where
    put_ bh (a,b) = do put_ bh a; put_ bh b
    get bh        = do a <- get bh
                       b <- get bh
                       return (a,b)

instance Binary a => Binary [a] where
    put_ bh l =
	do put_ bh (length l)
	   mapM (put_ bh) l
	   return ()
    get bh =
	do len <- get bh
	   mapM (\_ -> get bh) [1..(len::Int)]

instance Binary a => Binary (Maybe a) where
    put_ bh Nothing  = putByte bh 0
    put_ bh (Just a) = do putByte bh 1; put_ bh a
    get bh           = do h <- getWord8 bh
                          case h of
                            0 -> return Nothing
                            _ -> do x <- get bh; return (Just x)

-- Instances for FastPackedStrings
instance Binary P.ByteString where
    put_ bh@(BinIO ix_r h) ps@(P.PS _ptr _s l) = do
            put_ bh l   -- size
            ix <- readFastMutInt ix_r
            P.hPut h ps
            writeFastMutInt ix_r (ix+l)
            return ()

    get bh@(BinIO ix_r h) = do
            l  <- get bh
            ix <- readFastMutInt ix_r
            ps <- {-#SCC "Binary.ByteString.get" #-}P.hGet h l
            writeFastMutInt ix_r (ix+l)
            return $! ps

{-
            ps <- P.generate l $ \ptr -> do
                  let loop p n# | n# ==# l# = return ()
                                | otherwise = do
                                        c <- hGetChar h
                                        poke p (c2w c)
                                        loop (p `plusPtr` 1) (n# +# 1#)
                  loop ptr 0#
                  return l
            where
                c2w = fromIntegral . ord
-}

------------------------------------------------------------------------
-- FastMutInt
--
data FastMutInt = FastMutInt !(MutableByteArray# RealWorld)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s', arr #) ->
  (# s', FastMutInt arr #) }
  where I# size = SIZEOF_HSINT

readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s', i #) ->
  (# s', I# i #) }

writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s' ->
  (# s', () #) }
