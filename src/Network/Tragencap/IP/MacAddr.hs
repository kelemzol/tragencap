
module Network.Tragencap.IP.MacAddr where

import Data.Word

import Network.Tragencap.Core
import Network.Tragencap.Prelude


-- * Mac Address
---------------------------------------

data MacAddr
  = MACAddr
    { mac_1  :: Word8
    , mac_2  :: Word8
    , mac_3  :: Word8
    , mac_4  :: Word8
    , mac_5  :: Word8
    , mac_6  :: Word8
    , mac_7  :: Word8
    , mac_8  :: Word8
    , mac_9  :: Word8
    , mac_10 :: Word8
    , mac_11 :: Word8
    , mac_12 :: Word8
    }
  deriving (Eq, Ord, Show)
