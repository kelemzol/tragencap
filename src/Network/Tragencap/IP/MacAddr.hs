
module Network.Tragencap.IP.MacAddr where

import Data.Word
import Data.Binary.Bits.Get
import Data.Binary.Get
import Control.Applicative

import Network.Tragencap.Core
import Network.Tragencap.Prelude


-- * Mac Address
---------------------------------------

data MacAddr
  = MacAddr
    { mac_1  :: Word8
    , mac_2  :: Word8
    , mac_3  :: Word8
    , mac_4  :: Word8
    , mac_5  :: Word8
    , mac_6  :: Word8
    }
  deriving (Eq, Ord, Show)

macAddrGetter :: Get MacAddr
macAddrGetter = runBitGet $ block (MacAddr <$> word8 8 <*> word8 8 <*> word8 8 <*> word8 8 <*> word8 8 <*> word8 8)

instance SimpleParser MacAddr where
    simpleParser = get2parser macAddrGetter