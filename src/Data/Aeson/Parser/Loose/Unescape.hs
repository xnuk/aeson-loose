{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE PackageImports           #-}

-- | Nothing
module Data.Aeson.Parser.Loose.Unescape (
  unescapeText
) where

import "base" Control.Exception (evaluate, throw, try)
import "base" Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import "bytestring" Data.ByteString as B
import "bytestring" Data.ByteString.Internal as B hiding (c2w)
import qualified "text" Data.Text.Array as A
import "text" Data.Text.Encoding.Error (UnicodeException (..))
import "text" Data.Text.Internal (Text (..))
import "text" Data.Text.Internal.Private (runText)
import "text" Data.Text.Unsafe (unsafeDupablePerformIO)
import "base" Data.Word (Word8)
import "base" Foreign.C.Types (CInt (..), CSize (..))
import "base" Foreign.ForeignPtr (withForeignPtr)
import "base" Foreign.Marshal.Utils (with)
import "base" Foreign.Ptr (Ptr, plusPtr)
import "base" Foreign.Storable (peek)
import GHC.Base (MutableByteArray#)

foreign import ccall unsafe "_js_decode_string" c_js_decode
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO CInt

unescapeText' :: ByteString -> Text
unescapeText' (PS fp off len) = runText $ \done -> do
  let go dest = withForeignPtr fp $ \ptr ->
        with (0::CSize) $ \destOffPtr -> do
          let end = ptr `plusPtr` (off + len)
              loop curPtr = do
                res <- c_js_decode (A.maBA dest) destOffPtr curPtr end
                case res of
                  0 -> do
                    n <- peek destOffPtr
                    unsafeSTToIO (done dest (fromIntegral n))
                  _ ->
                    throw (DecodeError desc Nothing)
          loop (ptr `plusPtr` off)
  (unsafeIOToST . go) =<< A.new len
 where
  desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
{-# INLINE unescapeText' #-}

-- | Nothing
unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'
{-# INLINE unescapeText #-}
