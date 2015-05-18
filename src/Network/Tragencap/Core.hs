
{-# LANGUAGE KindSignatures
           , DataKinds
           , TypeFamilies
           , TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

module Network.Tragencap.Core where

import qualified Data.ByteString.Lazy as BS


-- | Encapcuale operator
--   encr - encapsulator
--   encd - encapsulated
data encr :< encd = encr :< encd
  deriving (Eq, Ord, Show)

infixl 5 :<



class Serial t where
    serial :: t -> BS.ByteString
    deserial :: BS.ByteString -> t



data l :? r = Inl l | Inr r
  deriving (Eq, Ord, Show)

infixl 6 :?



-- | Layer dependent matcher
class sub :-> sup where
    inj :: sub -> sup
    prj :: sup -> Maybe sub

instance a :-> a where
    inj = id
    prj = Just

instance a :-> (a :? b) where
    inj = Inl
    prj (Inl a) = Just a
    prj (Inr _) = Nothing

instance (a :-> b) => a :-> (b :? c) where
    inj = Inl . inj
    prj (Inl a) = prj a
    prj (Inr _) = Nothing


encapsulate :: (a :-> p) => e -> a -> e :< p
encapsulate e = (e :<) . inj

(<<) a b = encapsulate a b


-- | Tunneling protocols
--   Fixing a protocol type
data Tunnel p = Tunnel p
  deriving (Eq, Ord, Show)
