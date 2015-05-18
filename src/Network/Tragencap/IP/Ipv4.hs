
module Network.Tragencap.IP.Ipv4 where

import qualified Data.ByteString.Lazy as BS
import Data.Word

import Network.Tragencap.Core
import Network.Tragencap.Prelude


-- * Ipv4 Address

data Ipv4Addr
  = Ipv4Addr
    { ipv4_1            :: Word8
    , ipv4_2            :: Word8
    , ipv4_3            :: Word8
    , ipv4_4            :: Word8
    }
  deriving (Eq, Ord, Show)

-- FIXME: check http://en.wikipedia.org/wiki/IPv4#Packet_structure
data Ipv4
  = Ipv4
    { ipv4Version       :: Word8
    , ipv4Ihl           :: Word8
    , ipv4Dscp          :: Word8
    , ipv4Ecn           :: Word8
    , ipv4TotalLength   :: Word16
    , ipv4Identification :: Word16
    , ipv4Flags         :: Word8
    , ipv4FragmantOffset :: Word16
    , ipv4TimeToLive    :: Word8
    , ipv4Protocol      :: Word8 -- FIXME: http://en.wikipedia.org/wiki/IPv4#Data
    , ipv4HeaderChecksum :: Word16
    , ipv4Source        :: Ipv4Addr
    , ipv4Dest          :: Ipv4Addr
    , ipv4Options       :: BS.ByteString -- FIXME
    }
  deriving (Eq, Ord, Show)
