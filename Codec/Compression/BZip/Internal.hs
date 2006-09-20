-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level bzlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip.Internal (

  -- * Compression and decompression
  compressDefault,
  decompressDefault,
  Stream.BlockSize(..),

  -- * The same but with the full set of parameters
  compressFull,
  decompressFull,
  Stream.WorkFactor(..),
  Stream.MemoryLevel(..),

  ) where

import Prelude hiding (length)
import Control.Monad (liftM, when)
import Control.Exception (assert)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Base as Base
import Data.ByteString.Base (LazyByteString(LPS))

import qualified Codec.Compression.BZip.Stream as Stream
import Codec.Compression.BZip.Stream (Stream)

compressDefault
  :: Stream.BlockSize
  -> Lazy.ByteString
  -> Lazy.ByteString
compressDefault blockSize =
  compressFull blockSize
               Stream.Silent
               Stream.DefaultWorkFactor

decompressDefault
  :: Lazy.ByteString
  -> Lazy.ByteString
decompressDefault =
  decompressFull Stream.Silent
                 Stream.DefaultMemoryLevel

{-# NOINLINE compressFull #-}
compressFull
  :: Stream.BlockSize
  -> Stream.Verbosity
  -> Stream.WorkFactor
  -> Lazy.ByteString
  -> Lazy.ByteString

compressFull blockSize verbosity workFactor (LPS chunks) =
  Stream.run $ do
    Stream.compressInit blockSize verbosity workFactor
    case chunks of
      [] -> liftM LPS (fillBuffers [])
      (Base.PS inFPtr offset length : chunks') -> do
        Stream.pushInputBuffer inFPtr offset length
        liftM LPS (fillBuffers chunks')

  where
  outChunkSize :: Int
  outChunkSize = 32 * 1024 - 16

    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers ::
      [Strict.ByteString]
   -> Stream [Strict.ByteString]
  fillBuffers inChunks = do
    Stream.consistencyCheck
    Stream.trace "fillBuffers"
    Stream.dump

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      Stream.trace "no space in output buffer"
      outFPtr <- Stream.unsafeLiftIO (Base.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize
      Stream.trace "pushed output buffer"
      Stream.dump

    if inputBufferEmpty
      then case inChunks of
             [] -> drainBuffers []
             (Base.PS inFPtr offset length : inChunks') -> do
                Stream.trace "no space in input buffer"
                Stream.pushInputBuffer inFPtr offset length
                Stream.trace "pushed input buffer"
                Stream.dump
                drainBuffers inChunks'
      else drainBuffers inChunks

  drainBuffers ::
      [Strict.ByteString]
   -> Stream [Strict.ByteString]
  drainBuffers inChunks = do
    Stream.trace "drainBuffers"

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can't get BufferError since
    -- we can always make forward progress

    let action = if null inChunks then Stream.Finish else Stream.Run
    status <- Stream.compress action

    Stream.trace $ "compress: status = " ++ show status
    Stream.dump

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do Stream.trace "whole output buffer available, poping buffer"
                  (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.trace "poped buffer"
                  Stream.dump
                  Stream.trace "suspend outChunks"
                  outChunks <- Stream.unsafeInterleave (Stream.trace "force outChunks"
                               >> fillBuffers inChunks)
                  Stream.trace "return chunk"
                  return (Base.PS outFPtr offset length : outChunks)
          else do Stream.trace "output buffer not full, refilling"
                  fillBuffers inChunks

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do Stream.trace "poping last partial buffer"
                  Stream.dump
                  (outFPtr, offset, length) <- Stream.popOutputBuffer
                  return (Base.PS outFPtr offset length : [])
          else do Stream.trace "no final buffer. Done!"
                  Stream.dump
                  return []


{-# NOINLINE decompressFull #-}
decompressFull
  :: Stream.Verbosity
  -> Stream.MemoryLevel
  -> Lazy.ByteString
  -> Lazy.ByteString
decompressFull verbosity memLevel (LPS chunks) =
  Stream.run $ do
    Stream.decompressInit verbosity memLevel
    case chunks of
      [] -> liftM LPS (fillBuffers [])
      (Base.PS inFPtr offset length : chunks') -> do
        Stream.pushInputBuffer inFPtr offset length
        liftM LPS (fillBuffers chunks')

  where
  outChunkSize :: Int
  outChunkSize = 32 * 1024 - 16

    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers ::
      [Strict.ByteString]
   -> Stream [Strict.ByteString]
  fillBuffers inChunks = do
    Stream.trace "fillBuffers"
    Stream.dump

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      Stream.trace "no space in output buffer"
      outFPtr <- Stream.unsafeLiftIO (Base.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize
      Stream.trace "pushed output buffer"
      Stream.dump

    if inputBufferEmpty
      then case inChunks of
             [] -> do Stream.trace "inChunks empty"
                      drainBuffers []
             (Base.PS inFPtr offset length : inChunks') -> do
                Stream.trace "no space in input buffer"
                Stream.pushInputBuffer inFPtr offset length
                Stream.trace "pushed input buffer"
                Stream.dump
                drainBuffers inChunks'
      else drainBuffers inChunks


  drainBuffers ::
      [Strict.ByteString]
   -> Stream [Strict.ByteString]
  drainBuffers inChunks = do
    Stream.trace "drainBuffers"

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (null inChunks || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can't get a BufferError since
    -- we can always make forward progress

    status <- Stream.decompress

    Stream.trace $ "decompress: status = " ++ show status
    Stream.dump

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do Stream.trace "output buffer full, poping buffer"
                  (outFPtr, offset, length) <- Stream.popOutputBuffer
                  Stream.trace "poped buffer"
                  Stream.dump
                  Stream.trace "suspend outChunks"
                  outChunks <- Stream.unsafeInterleave (Stream.trace "force outChunks"
                              >> fillBuffers inChunks)
                  Stream.trace "return chunk"
                  return (Base.PS outFPtr offset length : outChunks)
          else do Stream.trace "output buffer not full, refilling"
                  fillBuffers inChunks

      Stream.StreamEnd -> do
        -- Note that there may be input bytes still available if the stream
        -- is embeded in some other data stream. Here we just silently discard
        -- any trailing data.
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do Stream.trace "poping last partial buffer"
                  Stream.dump
                  (outFPtr, offset, length) <- Stream.popOutputBuffer
                  return (Base.PS outFPtr offset length : [])
          else do Stream.trace "no final buffer. Done!"
                  Stream.dump
                  return []
