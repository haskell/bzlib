{-# LANGUAGE CPP, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level bzlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip.Internal (
  -- * Pure interface
  compress,
  decompress,

  -- * Monadic incremental interface
  -- $incremental-compression

  -- ** Incremental compression
  -- $using-incremental-compression

  CompressStream(..),
  compressST,
  compressIO,
  foldCompressStream,
  foldCompressStreamWithInput,

  -- ** Incremental decompression
  -- $using-incremental-decompression

  DecompressStream(..),
  DecompressError(..),
  decompressST,
  decompressIO,
  foldDecompressStream,
  foldDecompressStreamWithInput,

  -- * The compression parameter types
  CompressParams(..),
  defaultCompressParams,
  DecompressParams(..),
  defaultDecompressParams,
  Stream.BlockSize(..),
  Stream.WorkFactor(..),
  Stream.MemoryLevel(..),
  ) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Exception (Exception, throw, assert)
import Control.Monad.ST.Lazy hiding (stToIO)
import Control.Monad.ST.Strict (stToIO)
import qualified Control.Monad.ST.Unsafe as Unsafe (unsafeIOToST)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import GHC.IO (noDuplicate)

import qualified Codec.Compression.BZip.Stream as Stream
import Codec.Compression.BZip.Stream (Stream)

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
--
-- The 'compressBufferSize' is the size of the first output buffer containing
-- the compressed data. If you know an approximate upper bound on the size of
-- the compressed data then setting this parameter can save memory. The default
-- compression output buffer size is @16k@. If your estimate is wrong it does
-- not matter too much, the default buffer size will be used for the remaining
-- chunks.
--
data CompressParams = CompressParams {
  compressBlockSize   :: Stream.BlockSize,
  compressWorkFactor  :: Stream.WorkFactor,
  compressBufferSize  :: Int
} deriving (Show)

-- | The full set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
--
-- The 'decompressBufferSize' is the size of the first output buffer,
-- containing the uncompressed data. If you know an exact or approximate upper
-- bound on the size of the decompressed data then setting this parameter can
-- save memory. The default decompression output buffer size is @32k@. If your
-- estimate is wrong it does not matter too much, the default buffer size will
-- be used for the remaining chunks.
--
-- One particular use case for setting the 'decompressBufferSize' is if you
-- know the exact size of the decompressed data and want to produce a strict
-- 'Data.ByteString.ByteString'. The compression and decompression functions
-- use lazy 'Data.ByteString.Lazy.ByteString's but if you set the
-- 'decompressBufferSize' correctly then you can generate a lazy
-- 'Data.ByteString.Lazy.ByteString' with exactly one chunk, which can be
-- converted to a strict 'Data.ByteString.ByteString' in @O(1)@ time using
-- @'Data.ByteString.concat' . 'Data.ByteString.Lazy.toChunks'@.
--
data DecompressParams = DecompressParams {
  decompressMemoryLevel :: Stream.MemoryLevel,
  decompressBufferSize  :: Int
} deriving (Show)

-- | The default set of parameters for compression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {
  compressBlockSize   = Stream.DefaultBlockSize,
  compressWorkFactor  = Stream.DefaultWorkFactor,
  compressBufferSize  = defaultCompressBufferSize
}

-- | The default set of parameters for decompression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {
  decompressMemoryLevel = Stream.DefaultMemoryLevel,
  decompressBufferSize  = defaultDecompressBufferSize
}

-- | The default chunk sizes for the output of compression and decompression
-- are 16k and 32k respectively (less a small accounting overhead).
--
defaultCompressBufferSize, defaultDecompressBufferSize :: Int
defaultCompressBufferSize   = 16 * 1024 - L.chunkOverhead
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead

-- $incremental-compression
-- The pure 'Codec.Compression.BZip.Internal.compress' and
-- 'Codec.Compression.BZip.Internal.decompress' functions are streaming in the sense
-- that they can produce output without demanding all input, however they need
-- the input data stream as a lazy 'L.ByteString'. Having the input data
-- stream as a lazy 'L.ByteString' often requires using lazy I\/O which is not
-- appropriate in all circumstances.
--
-- For these cases an incremental interface is more appropriate. This interface
-- allows both incremental input and output. Chunks of input data are supplied
-- one by one (e.g. as they are obtained from an input source like a file or
-- network source). Output is also produced chunk by chunk.
--
-- The incremental input and output is managed via the 'CompressStream' and
-- 'DecompressStream' types. They represent the unfolding of the process of
-- compressing and decompressing. They operates in either the 'ST' or 'IO'
-- monads. They can be lifted into other incremental abstractions like pipes or
-- conduits, or they can be used directly in the following style.

-- $using-incremental-compression
--
-- In a loop:
--
--  * Inspect the status of the stream
--
--  * When it is 'CompressInputRequired' then you should call the action,
--    passing a chunk of input (or 'BS.empty' when no more input is available)
--    to get the next state of the stream and continue the loop.
--
--  * When it is 'CompressOutputAvailable' then do something with the given
--    chunk of output, and call the action to get the next state of the stream
--    and continue the loop.
--
--  * When it is 'CompressStreamEnd' then terminate the loop.
--
-- Note that you cannot stop as soon as you have no more input, you need to
-- carry on until all the output has been collected, i.e. until you get to
-- 'CompressStreamEnd'.
--
-- Here is an example where we get input from one file handle and send the
-- compressed output to another file handle.
--
-- > go :: Handle -> Handle -> CompressStream IO -> IO ()
-- > go inh outh (CompressInputRequired next) = do
-- >    inchunk <- BS.hGet inh 4096
-- >    go inh outh =<< next inchunk
-- > go inh outh (CompressOutputAvailable outchunk next) =
-- >    BS.hPut outh outchunk
-- >    go inh outh =<< next
-- > go _ _ CompressStreamEnd = return ()
--
-- The same can be achieved with 'foldCompressStream':
--
-- > foldCompressStream
-- >   (\next -> do inchunk <- BS.hGet inh 4096; next inchunk)
-- >   (\outchunk next -> do BS.hPut outh outchunk; next)
-- >   (return ())

-- $using-incremental-decompression
--
-- The use of 'DecompressStream' is very similar to 'CompressStream' but with
-- a few differences:
--
-- * There is the extra possibility of a 'DecompressStreamError'
--
-- * There can be extra trailing data after a compressed stream, and the
--   'DecompressStreamEnd' includes that.
--
-- Otherwise the same loop style applies, and there are fold functions.

-- | The unfolding of the compression process, where you provide a sequence
-- of uncompressed data chunks as input and receive a sequence of compressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
--
data CompressStream m =
     CompressInputRequired {
         compressSupplyInput :: S.ByteString -> m (CompressStream m)
       }

   | CompressOutputAvailable {
        compressOutput :: !S.ByteString,
        compressNext   :: m (CompressStream m)
      }

   | CompressStreamEnd

-- | A fold over the 'CompressStream' in the given monad.
--
-- One way to look at this is that it runs the stream, using callback functions
-- for the three stream events.
--
foldCompressStream 
  :: Monad m
  => ((S.ByteString -> m a) -> m a)
  -- ^ How to obtain more input to be compressed.
  -- Typically, this is a lambda of the form
  -- 
  -- > \consume -> do { bs <- obtainData ; consume bs }
  -- 
  -> (S.ByteString -> m a -> m a)
  -- ^ The right-folding operation. Note that the 
  -- second argument is already embedded in the
  -- monad. This is typically a lambda of the form
  -- 
  -- > \chunk next -> L.chunk chunk <$> next
  -- 
  -- or
  --
  -- > \chunk next -> do { writeData chunk ; next}
  -- 
  -> m a
  -- ^ The base value of the fold. If the output
  -- is itself a 'L.ByteString', this can just
  -- be (return 'L.empty').
  -> CompressStream m 
  -- ^ The input stream. Typically, this is
  -- ('compressIO' params) or ('compressST' params),
  -- depending on the choice of monad.
  -> m a
foldCompressStream input output end = fold
  where
    fold (CompressInputRequired next) =
      input (\x -> next x >>= fold)

    fold (CompressOutputAvailable outchunk next) =
      output outchunk (next >>= fold)

    fold CompressStreamEnd =
      end

-- | A variant on 'foldCompressStream' that is pure rather than operating in a
-- monad and where the input is provided by a lazy 'L.ByteString'. So we only
-- have to deal with the output and end parts, making it just like a foldr on a
-- list of output chunks.
--
-- For example:
--
-- > toChunks = foldCompressStreamWithInput (:) []
--
foldCompressStreamWithInput 
  :: (S.ByteString -> a -> a)
  -- ^ The right-folding operation, used to create output.
  -- In typical usage, this is 'L.chunk'.
  -> a
  -- ^ The base value of the fold. In typical usage,
  -- this is just 'L.empty' or 'mempty'.
  -> (forall s. CompressStream (ST s))
  -- ^ The compression stream. Typically, this is
  -- ('compressST' params).
  -> L.ByteString
  -- ^ The input lazy 'L.ByteString'.
  -> a
foldCompressStreamWithInput chunk end = \s lbs ->
    runST (fold s (L.toChunks lbs))
  where
    fold (CompressInputRequired next) [] =
      next S.empty >>= \strm -> fold strm []

    fold (CompressInputRequired next) (inchunk:inchunks) =
      next inchunk >>= \s -> fold s inchunks

    fold (CompressOutputAvailable outchunk next) inchunks = do
      r <- next >>= \s -> fold s inchunks
      return $ chunk outchunk r

    fold CompressStreamEnd _inchunks =
      return end

-- | Compress a data stream provided as a lazy 'L.ByteString'.
--
-- There are no expected error conditions. All input data streams are valid. It
-- is possible for unexpected errors to occur, such as running out of memory,
-- or finding the wrong version of the bz2 C library; these are thrown as
-- exceptions.
--
compress   :: CompressParams -> L.ByteString -> L.ByteString

-- | Incremental compression in the 'ST' monad. Using 'ST' makes it possible
-- to write pure /lazy/ functions while making use of incremental compression.
--
-- Chunk size must fit into t'CUInt'.
compressST :: CompressParams -> CompressStream (ST s)

-- | Incremental compression in the 'IO' monad.
--
-- Chunk size must fit into t'CUInt'.
compressIO :: CompressParams -> CompressStream IO

compress   params = foldCompressStreamWithInput
                      L.Chunk L.Empty
                      (compressStreamST params)
compressST params = compressStreamST params
compressIO params = compressStreamIO params

-- | Chunk size must fit into t'CUInt'.
compressStream
  :: CompressParams -> S.ByteString -> Stream (CompressStream Stream)
compressStream (CompressParams blockSize workFactor initChunkSize) =
    \chunk -> do
      Stream.compressInit blockSize Stream.Silent workFactor
      case chunk of
        _ | S.null chunk -> fillBuffers 14 --bzip2 header is 14 bytes
        S.PS inFPtr offset length -> do
          Stream.pushInputBuffer inFPtr offset length
          fillBuffers initChunkSize
  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int -> Stream (CompressStream Stream)
  fillBuffers outChunkSize = do
#ifdef DEBUG
    Stream.consistencyCheck
#endif

    -- in this state there are two possibilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then return $ CompressInputRequired $ \chunk ->
           case chunk of
           _ | S.null chunk          -> drainBuffers True
           S.PS inFPtr offset length -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers False
      else drainBuffers False


  drainBuffers :: Bool -> Stream (CompressStream Stream)
  drainBuffers lastChunk = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (lastChunk || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress

    let action = if lastChunk then Stream.Finish else Stream.Run
    status <- Stream.compress action

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  return $ CompressOutputAvailable chunk $ do
                    fillBuffers defaultCompressBufferSize
          else do fillBuffers defaultCompressBufferSize

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  Stream.finalise
                  return $ CompressOutputAvailable chunk (return CompressStreamEnd)
          else do Stream.finalise
                  return CompressStreamEnd

      Stream.Error _ msg -> fail msg

-- | The unfolding of the decompression process, where you provide a sequence
-- of compressed data chunks as input and receive a sequence of uncompressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
--
data DecompressStream m =

     DecompressInputRequired {
         decompressSupplyInput :: S.ByteString -> m (DecompressStream m)
       }

   | DecompressOutputAvailable {
         decompressOutput :: !S.ByteString,
         decompressNext   :: m (DecompressStream m)
       }

   -- | Includes any trailing unconsumed /input/ data.
   | DecompressStreamEnd {
         decompressUnconsumedInput :: S.ByteString
       }

   -- | An error code
   | DecompressStreamError {
         decompressStreamError :: DecompressError
       }

-- | The possible error cases when decompressing a stream.
--
-- This can be 'show'n to give a human readable error message.
--
-- @since 0.5.3.0
data DecompressError =
     -- | The compressed data stream ended prematurely. This may happen if the
     -- input data stream was truncated.
     TruncatedInput
     -- | If the compressed data stream is corrupted in any way then you will
     -- get this error.
   | DataFormatError String

instance Show DecompressError where
  show TruncatedInput     = modprefix "premature end of compressed data stream"
  show (DataFormatError detail) = modprefix ("compressed data stream format error (" ++ detail ++ ")")

modprefix :: ShowS
modprefix = ("Codec.Compression.BZip: " ++)

instance Exception DecompressError

-- | A fold over the 'DecompressStream' in the given monad.
--
-- One way to look at this is that it runs the stream, using callback functions
-- for the four stream events.
--
foldDecompressStream 
  :: Monad m
  => ((S.ByteString -> m a) -> m a)
  -- ^ How to obtain more input for the decompression
  -- stream. Typically, this is a lambda of the form
  -- 
  -- > \consume -> do { bs <- obtainData ; consume bs }
  -- 
  -> (S.ByteString -> m a -> m a)
  -- ^ The right-folding operation. Note that the 
  -- second argument is already embedded in the
  -- monad. This is typically a lambda of the form
  -- 
  -- > \chunk next -> L.chunk chunk <$> next
  -- 
  -- or
  --
  -- > \chunk next -> do { writeData chunk ; next}
  -- 
  -> (S.ByteString -> m a)
  -- ^ How to handle any trailing data after
  -- decompression is completed. To ignore it,
  -- just pass @const (return bas)@, where @bas@
  -- is the base value of the right-fold operation.
  -> (DecompressError -> m a)
  -- ^ How to handle errors. Typically, this is
  -- 'throw', but it can be e.g. (return . 'Left')
  -- if the output value is wrapped in 'Either'.
  -> DecompressStream m 
  -- ^ The input stream. Typically, this is
  -- ('decompressIO' params) or ('decompressST' params),
  -- depending on the choice of monad.
  -> m a
foldDecompressStream input output end err = fold
  where
    fold (DecompressInputRequired next) =
      input (\x -> next x >>= fold)

    fold (DecompressOutputAvailable outchunk next) =
      output outchunk (next >>= fold)

    fold (DecompressStreamEnd inchunk) = end inchunk
    fold (DecompressStreamError derr)  = err derr

-- | A variant on 'foldDecompressStream' that is pure rather than operating in a
-- monad and where the input is provided by a lazy 'L.ByteString'. So we only
-- have to deal with the output, end and error parts, making it like a foldr on
-- a list of output chunks.
--
-- For example:
--
-- > toChunks params = foldDecompressStreamWithInput (:) (const []) throw (decompressST params)
--
-- or
--
-- > import qualified Data.ByteString.Lazy          as L
-- > import qualified Data.ByteString.Lazy.Internal as L
-- >
-- > compressWith params = foldDecompressStreamWithInput (L.chunk) (const L.empty) throw (decompressST params)
--
foldDecompressStreamWithInput 
  :: (S.ByteString -> a -> a)
  -- ^ The right-folding operation, used to create output.
  -- In typical usage, this is 'L.chunk'.
  -> (L.ByteString -> a)
  -- ^ How to handle any trailing data; typically, this
  -- is discarded. 
  -> (DecompressError -> a)
  -- ^ How to handle any errors. To raise this as an
  -- error, just use 'throw'.
  -> (forall s. DecompressStream (ST s))
  -- ^ The decompression stream. Typically, this is
  -- ('decompressST' params).  
  -> L.ByteString
  -- ^ The input lazy `L.ByteString`.
  -> a
foldDecompressStreamWithInput chunk end err = \s lbs ->
    runST (fold s (L.toChunks lbs))
  where
    fold (DecompressInputRequired next) [] =
      next S.empty >>= \strm -> fold strm []

    fold (DecompressInputRequired next) (inchunk:inchunks) =
      next inchunk >>= \s -> fold s inchunks

    fold (DecompressOutputAvailable outchunk next) inchunks = do
      r <- next >>= \s -> fold s inchunks
      return $ chunk outchunk r

    fold (DecompressStreamEnd inchunk) inchunks =
      return $ end (L.fromChunks (inchunk:inchunks))

    fold (DecompressStreamError derr) _ =
      return $ err derr

-- | Decompress a data stream provided as a lazy 'L.ByteString'.
--
-- It will throw an exception if any error is encountered in the input data.
-- If you need more control over error handling then use one the incremental
-- versions, 'decompressST' or 'decompressIO'.
--
decompress   :: DecompressParams -> L.ByteString -> L.ByteString

-- | Incremental decompression in the 'ST' monad. Using 'ST' makes it possible
-- to write pure /lazy/ functions while making use of incremental decompression.
--
-- Chunk size must fit into t'CUInt'.
decompressST :: DecompressParams -> DecompressStream (ST s)

-- | Incremental decompression in the 'IO' monad.
--
-- Chunk size must fit into t'CUInt'.
decompressIO :: DecompressParams -> DecompressStream IO

decompress   params = foldDecompressStreamWithInput
                        L.Chunk (const L.Empty) throw
                        (decompressStreamST params)
decompressST params = decompressStreamST params
decompressIO params = decompressStreamIO params

-- | Chunk size must fit into t'CUInt'.
decompressStream
  :: DecompressParams -> S.ByteString -> Stream (DecompressStream Stream)
decompressStream (DecompressParams memLevel initChunkSize) =
    \chunk -> do
      Stream.decompressInit Stream.Silent memLevel
      case chunk of
        _ | S.null chunk -> fillBuffers 4 --always an error anyway
        S.PS inFPtr offset length -> do
          Stream.pushInputBuffer inFPtr offset length
          fillBuffers initChunkSize

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int -> Stream (DecompressStream Stream)
  fillBuffers outChunkSize = do

    -- in this state there are two possibilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then return $ DecompressInputRequired $ \chunk ->
           case chunk of
             _ | S.null chunk -> drainBuffers True
             S.PS inFPtr offset length -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers False
      else drainBuffers False


  drainBuffers :: Bool -> Stream (DecompressStream Stream)
  drainBuffers lastChunk = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (lastChunk || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress or at
    -- least detect premature EOF

    status <- Stream.decompress

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  return $ DecompressOutputAvailable chunk $ do
                    fillBuffers defaultDecompressBufferSize
          else do -- We need to detect if we ran out of input:
                  inputBufferEmpty <- Stream.inputBufferEmpty
                  if inputBufferEmpty && lastChunk
                    then return (DecompressStreamError TruncatedInput)
                    else fillBuffers defaultDecompressBufferSize

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        if inputBufferEmpty
          then do finish (DecompressStreamEnd S.empty)
          else do (inFPtr, offset, length) <- Stream.popRemainingInputBuffer
                  let inchunk = S.PS inFPtr offset length
                  finish (DecompressStreamEnd inchunk)

      Stream.Error code msg -> case code of
          Stream.DataError -> finish (DecompressStreamError (DataFormatError msg))
          _                -> fail msg

  finish end = do
    outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
    if outputBufferBytesAvailable > 0
      then do (outFPtr, offset, length) <- Stream.popOutputBuffer
              return (DecompressOutputAvailable (S.PS outFPtr offset length) (return end))
      else return end


------------------------------------------------------------------------------

mkStateST :: ST s (Stream.State s)
mkStateIO :: IO (Stream.State RealWorld)
mkStateST = strictToLazyST Stream.mkState
mkStateIO = stToIO Stream.mkState

runStreamST :: Stream a -> Stream.State s -> ST s (a, Stream.State s)
runStreamIO :: Stream a -> Stream.State RealWorld -> IO (a, Stream.State RealWorld)
runStreamST strm zstate = strictToLazyST (Unsafe.unsafeIOToST noDuplicate >> Stream.runStream strm zstate)
runStreamIO strm zstate = stToIO (Stream.runStream strm zstate)

compressStreamIO :: CompressParams -> CompressStream IO
compressStreamIO params =
    CompressInputRequired {
      compressSupplyInput = \chunk -> do
        zstate <- mkStateIO
        let next = compressStream params
        (strm', zstate') <- runStreamIO (next chunk) zstate
        return (go strm' zstate')
    }
  where
    go :: CompressStream Stream -> Stream.State RealWorld -> CompressStream IO
    go (CompressInputRequired next) zstate =
      CompressInputRequired {
        compressSupplyInput = \chunk -> do
          (strm', zstate') <- runStreamIO (next chunk) zstate
          return (go strm' zstate')
      }

    go (CompressOutputAvailable chunk next) zstate =
      CompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamIO next zstate
        return (go strm' zstate')

    go CompressStreamEnd _ = CompressStreamEnd


compressStreamST :: CompressParams -> CompressStream (ST s)
compressStreamST params =
    CompressInputRequired {
      compressSupplyInput = \chunk -> do
        zstate <- mkStateST
        let next = compressStream params
        (strm', zstate') <- runStreamST (next chunk) zstate
        return (go strm' zstate')
    }
  where
    go :: CompressStream Stream -> Stream.State s -> CompressStream (ST s)
    go (CompressInputRequired next) zstate =
      CompressInputRequired {
        compressSupplyInput = \chunk -> do
          (strm', zstate') <- runStreamST (next chunk) zstate
          return (go strm' zstate')
      }

    go (CompressOutputAvailable chunk next) zstate =
      CompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamST next zstate
        return (go strm' zstate')

    go CompressStreamEnd _ = CompressStreamEnd


decompressStreamIO :: DecompressParams -> DecompressStream IO
decompressStreamIO params =
      DecompressInputRequired $ \chunk -> do
        zstate <- mkStateIO
        let next = decompressStream params
        (strm', zstate') <- runStreamIO (next chunk) zstate
        go strm' zstate'
  where
    go :: DecompressStream Stream -> Stream.State RealWorld
       -> IO (DecompressStream IO)
    go (DecompressInputRequired next) zstate =
      return $ DecompressInputRequired $ \chunk -> do
        (strm', zstate') <- runStreamIO (next chunk) zstate
        go strm' zstate'

    go (DecompressOutputAvailable chunk next) zstate =
      return $ DecompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamIO next zstate
        go strm' zstate'

    go (DecompressStreamEnd unconsumed) zstate =
        finaliseStreamEnd unconsumed zstate

    go (DecompressStreamError err) zstate = finaliseStreamError err zstate

    finaliseStreamEnd unconsumed zstate = do
        _ <- runStreamIO Stream.finalise zstate
        return (DecompressStreamEnd unconsumed)

    finaliseStreamError err zstate = do
        _ <- runStreamIO Stream.finalise zstate
        return (DecompressStreamError err)


decompressStreamST :: DecompressParams -> DecompressStream (ST s)
decompressStreamST params =
      DecompressInputRequired $ \chunk -> do
        zstate <- mkStateST
        let next = decompressStream params
        (strm', zstate') <- runStreamST (next chunk) zstate
        go strm' zstate'
  where
    go :: DecompressStream Stream -> Stream.State s
       -> ST s (DecompressStream (ST s))
    go (DecompressInputRequired next) zstate =
      return $ DecompressInputRequired $ \chunk -> do
        (strm', zstate') <- runStreamST (next chunk) zstate
        go strm' zstate'

    go (DecompressOutputAvailable chunk next) zstate =
      return $ DecompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamST next zstate
        go strm' zstate'

    go (DecompressStreamEnd unconsumed) zstate =
        finaliseStreamEnd unconsumed zstate

    go (DecompressStreamError err) zstate = finaliseStreamError err zstate

    finaliseStreamEnd unconsumed zstate = do
        _ <- runStreamST Stream.finalise zstate
        return (DecompressStreamEnd unconsumed)

    finaliseStreamError err zstate = do
        _ <- runStreamST Stream.finalise zstate
        return (DecompressStreamError err)
