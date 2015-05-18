
module Network.Tragencap.IP.Udp where

import Data.Word

import Network.Tragencap.Core
import Network.Tragencap.Prelude


-- * UDP
---------------------------------------

data Udp
  = Udp
    { udpSource         :: Port
    , udpDest           :: Port
    , udpLength         :: Word16
    , udpCheckSum       :: Word16
    }
  deriving (Eq, Ord, Show)
