
module Network.Tragencap.IP.Icmp where

import Data.Word

import Network.Tragencap.Core
import Network.Tragencap.Prelude

-- * ICMP
---------------------------------------

-- FIXME: check http://en.wikipedia.org/wiki/Internet_Control_Message_Protocol
data Icmp
  = Icmp
    { icmpType          :: Word8
    , icmpCode          :: Word8
    , icmpCheckSum      :: Word16
    , icmpRestOfHeader  :: Word32
    }
  deriving (Eq, Ord, Show)
