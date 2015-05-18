
module Network.Tragencap.IP.Ethernet where

import qualified Data.ByteString.Lazy as BS

import Network.Tragencap.Core
import Network.Tragencap.Prelude
import Network.Tragencap.IP.MacAddr


-- * EtherType
---------------------------------------

-- FIXME: implement these: http://en.wikipedia.org/wiki/EtherType#Examples
data EtherType
  = EtherTypeIpv4
  | EtherTypeArp
  | EtherTypeWakeOnLan
  | EtherTypeIEEEStd1722_2011
  deriving (Eq, Ord, Show)

-- * Ethernet
---------------------------------------

data Ethernet
  = RawEthernet
    { ethernetDest      :: MacAddr
    , ethernetSource    :: MacAddr
    , ethernetEtherType :: EtherType
    }
  deriving (Eq, Ord, Show)

