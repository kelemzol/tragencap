
module Network.Tragencap.IP.Arp where

import qualified Data.ByteString.Lazy as BS
import Data.Word

import Network.Tragencap.Core
import Network.Tragencap.Prelude
import Network.Tragencap.IP.MacAddr
import Network.Tragencap.IP.Ipv4


-- * Arp
---------------------------------------

-- FIXME: check http://en.wikipedia.org/wiki/Address_Resolution_Protocol#Packet_structure
data Arp
  = RawARP
    { arpHType          :: Word16
    , arpPType          :: Word16
    , arpHLen           :: Word8
    , arpPLen           :: Word8
    , arpOper           :: Word16
    , arpSha            :: MacAddr
    , arpSpa            :: Ipv4Addr
    , arpTha            :: MacAddr
    , arpTpa            :: Ipv4Addr
    }
  deriving (Eq, Ord, Show)
