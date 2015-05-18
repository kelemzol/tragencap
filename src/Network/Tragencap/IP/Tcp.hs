
module Network.Tragencap.IP.Tcp where

import Data.Word

import Network.Tragencap.Core
import Network.Tragencap.Prelude


-- * TCP
---------------------------------------

data TcpFlags = TcpFlags
  deriving (Eq, Ord, Show)

data Tcp
  = Tcp
    { tcpSource         :: Port
    , tcpDest           :: Port
    , tcpSeqNum         :: Word32
    , tcpAckNum         :: Word32
    , tcpFlags          :: TcpFlags
    , tcpWinSize        :: Word16
    , tcpCheckSum       :: Word16
    , tcpUrg            :: Word16
    }
  deriving (Eq, Ord, Show)
