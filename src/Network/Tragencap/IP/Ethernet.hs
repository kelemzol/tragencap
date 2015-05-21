
{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           #-}

module Network.Tragencap.IP.Ethernet where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Bits.Get
import Data.Binary.Get
import Data.Word
import Control.Applicative

import Network.Tragencap.Core
import Network.Tragencap.Prelude
import Network.Tragencap.IP.MacAddr
import Network.Tragencap.IP.Ipv4
import Network.Tragencap.IP.Arp


-- * EtherType
---------------------------------------

-- FIXME: implement these: http://en.wikipedia.org/wiki/EtherType#Examples
data EtherType
  = EtherTypeIpv4
  | EtherTypeArp
  | OtherEtherType Word16
--  | EtherTypeWakeOnLan
--  | EtherTypeIEEEStd1722_2011
  deriving (Eq, Ord, Show)

-- * Ethernet
---------------------------------------

data Ethernet
  = Ethernet
    { ethernetDest      :: MacAddr
    , ethernetSource    :: MacAddr
    , ethernetEtherType :: EtherType
    }
  deriving (Eq, Ord, Show)

word2ett :: Word16 -> EtherType
word2ett 0x0800 = EtherTypeIpv4
word2ett 0x0806 = EtherTypeArp
word2ett other = OtherEtherType other


etherTypeGetter :: Get EtherType
etherTypeGetter = runBitGet (block (word2ett <$> word16be 16))

ethernetGetter :: Get Ethernet
ethernetGetter = Ethernet <$> macAddrGetter <*> macAddrGetter <*> etherTypeGetter


instance SimpleParser Ethernet where
    simpleParser = get2parser ethernetGetter

instance ( Ipv4 :! k
         , {-Arp :! k
         , -}Payload :! k
         ) => GetEncapsulatedParser Ethernet k where
    getEncapsulatedParser ethernet = case ethernetEtherType ethernet of
        --EtherTypeIpv4 -> liftParser ipv4Parser
        --EtherTypeArp -> liftParser arpParser
        OtherEtherType _ -> liftParser payloadParser

ipv4Parser :: Parser Ipv4
ipv4Parser = undefined

arpParser :: Parser Arp
arpParser = undefined

instance SimpleParser Ipv4 where
    simpleParser = ipv4Parser

test :: SimpleParser (Ethernet :< (Payload :? Ipv4)) => a
test = undefined

test2 :: (SimpleParser Ethernet, SimpleParser Payload, SimpleParser Ipv4) => a
test2 = undefined

test3 :: (SimpleParser (Payload :? Ipv4)) => a
test3 = undefined


