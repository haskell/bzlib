-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the bzip2 format.
--
-- bzip2 is a freely available, patent free, high-quality data compressor. It
-- typically compresses files to within 10% to 15% of the best available
-- techniques (the PPM family of statistical compressors), whilst being around
-- twice as fast at compression and six times faster at decompression.
--
-- <http://www.bzip.org/>
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip (

  -- | This module provides pure functions for compressing and decompressing
  -- streams of data represented by lazy 'ByteString's. This makes it easy to
  -- use either in memory or with disk or network IO.
  --
  -- For example a simple bzip compression program is just:
  --
  -- > import qualified Data.ByteString.Lazy as ByteString
  -- > import qualified Codec.Compression.BZip as BZip
  -- >
  -- > main = ByteString.interact BZip.compress
  --
  -- Or you could lazily read in and decompress a @.bz2@ file using:
  --
  -- > content <- fmap BZip.decompress (readFile file)
  --

  -- * Compression
  compress,
  compressWith,
  BlockSize(..),

  -- * Decompression
  decompress

  ) where

import Data.ByteString.Lazy (ByteString)

import Codec.Compression.BZip.Internal as Internal


-- | Decompress a stream of data in the bzip2 format.
--
-- There are a number of errors that can occur. In each case an exception will
-- be thrown. The possible error conditions are:
--
-- * if the stream does not start with a valid gzip header
--
-- * if the compressed stream is corrupted
--
-- * if the compressed stream ends permaturely
--
-- Note that the decompression is performed /lazily/. Errors in the data stream
-- may not be detected until the end of the stream is demanded (since it is
-- only at the end that the final checksum can be checked). If this is
-- important to you, you must make sure to consume the whole decompressed
-- stream before doing any IO action that depends on it.
--
decompress :: ByteString -> ByteString
decompress = Internal.decompressDefault


-- | Compress a stream of data into the bzip2 format.
--
-- This uses the default compression level which uses the largest compression
-- block size for the highest compression level. Use 'compressWith' to adjust
-- the compression block size.
--
compress :: ByteString -> ByteString
compress = Internal.compressDefault DefaultBlockSize


-- | Like 'compress' but with an extra parameter to specify the block size
-- used for compression.
--
compressWith :: BlockSize -> ByteString -> ByteString
compressWith = Internal.compressDefault
