
{-# LANGUAGE KindSignatures
           , DataKinds
           , TypeFamilies
           , TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , ScopedTypeVariables
           #-}

module Network.Tragencap.Core where

import Control.Applicative
import Data.Either

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


class Parse t where
    parse :: BS.ByteString -> Maybe (BS.ByteString, t)


-- FIXME: directed parsig: parse of encr -> parser of encd
instance (Parse a, Parse b) => Parse (a :? b) where
    parse str =     liftA (\ (bs, c) -> (bs, Inl c)) (parse str :: Maybe (BS.ByteString, a))
                <|> liftA (\ (bs, c) -> (bs, Inr c)) (parse str :: Maybe (BS.ByteString, b))

instance (Parse a, Parse b) => Parse (a :< b) where
    parse str = do
        (str', encr) <- parse str
        (str'', encd) <- parse str'
        return (str'', encr :< encd)


data l :? r = Inl l | Inr r
  deriving (Eq, Ord, Show)

infixl 6 :?



-- | Layer dependent matcher
class sub :! sup where
    ainj :: sub -> sup
    -- ^ alternative injection
    aprj :: sup -> Maybe sub
    -- ^ alternative projection

instance a :! a where
    ainj = id
    aprj = Just

instance a :! (a :? b) where
    ainj = Inl
    aprj (Inl a) = Just a
    aprj (Inr _) = Nothing

instance (a :! c) => a :! (b :? c) where
    ainj = Inr . ainj
    aprj (Inr a) = aprj a
    aprj (Inl _) = Nothing

class (sub :-> sup) su where
    einj :: sub -> (su -> sup)
    eprj :: sup -> (su, sub)

instance (a :-> a) a where
    einj a = id
    eprj a = (a, a)

instance (a :-> (b :< a)) b where
    einj a = (:< a)
    eprj (b :< a) = (b, a)

instance ((a :-> b) c) => (a :-> (e :< b)) (e :< c) where
    einj a (e :< b) = e :< (einj a b)
    eprj (e :< b) = let (su, sub) = eprj b in (e :< su, sub)


encapsulate :: (a :! p) => e -> a -> e :< p
encapsulate e = (e :<) . ainj

(<<) a b = encapsulate a b


-- | Tunneling protocols
--   Fixing a protocol type
data Tunnel p = Tunnel p
  deriving (Eq, Ord, Show)
