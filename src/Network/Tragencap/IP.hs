
{-# LANGUAGE TypeOperators
           #-}

module Network.Tragencap.IP
  ( module Network.Tragencap.IP.Icmp
  , module Network.Tragencap.IP.Tcp
  , module Network.Tragencap.IP.Udp
  , module Network.Tragencap.IP.Arp
  , module Network.Tragencap.IP.Ipv4
  , module Network.Tragencap.IP.Ethernet
  , module Network.Tragencap.IP.MacAddr
  ) where

import Network.Tragencap.IP.Icmp
import Network.Tragencap.IP.Tcp
import Network.Tragencap.IP.Udp
import Network.Tragencap.IP.Arp
import Network.Tragencap.IP.Ipv4
import Network.Tragencap.IP.Ethernet
import Network.Tragencap.IP.MacAddr

import Network.Tragencap.Core
import Network.Tragencap.Prelude

type StdIPProtocol = LinkLayer

type LinkLayer = Ethernet :< (Payload :? InternetLayer) :? Arp

type InternetLayer = Ipv4 :< (Payload :? TransportLayer) :? Icmp

type TransportLayer = Udp :< (Payload :? ApplicationLayer) :? Tcp :< (Payload :? ApplicationLayer)

type ApplicationLayer = ()


type TunneledIP = Ethernet :< (Tunnel TunneledIP)
