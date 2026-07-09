module Main where

import Codec.Compression.BZip.Internal (
  CompressParams (..),
  DecompressParams (..),
  DecompressStream (..),
  compress,
  decompress,
  decompressIO,
  defaultDecompressParams,
 )
import Control.Monad (liftM2)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Test.Codec.Compression.BZip.Internal ()
import Test.Codec.Compression.BZip.Stream ()
import Test.QuickCheck (Property, Testable (..), ioProperty, (==>))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Utils ()

main :: IO ()
main =
  defaultMain $
    testGroup
      "bzip tests"
      [ testProperty "decompress . compress = id (standard)" prop_decompress_after_compress
      , testProperty "decoding 66 does not throw" $ decodeByte 66
      , testProperty "decoding 67 does not throw" $ decodeByte 67
      ]

prop_decompress_after_compress
  :: CompressParams
  -> DecompressParams
  -> Property
prop_decompress_after_compress cp dp =
  decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
    liftM2 (==) (decompress dp . compress cp) id

decodeByte :: Word8 -> Property
decodeByte w8 = case decompressIO defaultDecompressParams of
  DecompressInputRequired cont -> ioProperty $ do
    state <- cont (B.singleton w8)
    pure $ case state of
      DecompressInputRequired {} -> property True
      DecompressOutputAvailable {} -> property False
      DecompressStreamEnd {} -> property False
      DecompressStreamError {} -> property True
  _ -> property False
