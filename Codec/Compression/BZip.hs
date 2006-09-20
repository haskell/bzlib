-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the bzip2 format.
--
-- bzip2 is a freely available, patent free (see below), high-quality data
-- compressor. It typically compresses files to within 10% to 15% of the best
-- available techniques (the PPM family of statistical compressors), whilst
-- being around twice as fast at compression and six times faster at
-- decompression.
--
-- <http://www.bzip.org/>
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip (

  -- * Compression
  compress,
  compressWith,
  BlockSize(..),

  -- * Decompression
  decompress

  ) where

import Data.ByteString.Lazy (ByteString)

import Codec.Compression.BZip.Internal as Internal

decompress :: ByteString -> ByteString
decompress = Internal.decompressDefault

compress :: ByteString -> ByteString
compress = Internal.compressDefault DefaultBlockSize

compressWith :: BlockSize -> ByteString -> ByteString
compressWith = Internal.compressDefault
