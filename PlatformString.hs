{-# LANGUAGE CPP #-}
{-
Cross-platform, cross-GHC-version en/decoding of file paths, command-line arguments etc.

Provides the PlatformString type alias, representing a possibly encoded
string received from the OS environment or intended for it; and functions
to convert these to and from regular non-encoded strings, which should
work correctly on most platforms and GHC versions.

-}

module PlatformString (
  PlatformString,
  fromPlatformString,
  toPlatformString
)
where

#if __GLASGOW_HASKELL__ < 702
import Codec.Binary.UTF8.String as UTF8 (decodeString, encodeString, isUTF8Encoded)
import System.Info (os)
#endif

-- | A platform string is a string value received from or intended for the
-- (current) operating system, such as a file path or command-line
-- argument (or an environment variable's name or value ?).  On some
-- platforms (unix) these are typically encoded, and on others (windows)
-- they are not.  On unix we assume UTF-8 encoding, as recommended by
-- http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
--
type PlatformString = String

fromPlatformString :: PlatformString -> String
toPlatformString :: String -> PlatformString

#if __GLASGOW_HASKELL__ < 702
fromPlatformString s = if UTF8.isUTF8Encoded s then UTF8.decodeString s else s
toPlatformString = case os of
                     "unix" -> UTF8.encodeString
                     "linux" -> UTF8.encodeString
                     "darwin" -> UTF8.encodeString
                     _ -> id
#else
fromPlatformString = id
toPlatformString = id
#endif
