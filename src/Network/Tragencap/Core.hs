
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
import Data.Binary.Get

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

type ParserResult t = Maybe (BS.ByteString, t)
data Parser t = Parser { runParser :: BS.ByteString -> ParserResult t }

get2parser :: Get a -> Parser a
get2parser get = Parser parser
  where
    parser str = case runGetOrFail get str of
        (Left _) -> Nothing
        (Right (bs, _, a)) -> Just (bs, a)

liftParser :: (k :! t) => Parser k -> Parser t
liftParser (Parser p) = Parser (\ bs -> lifting (p bs))
  where
    lifting (Just (bs', k)) = Just (bs', ainj k)
    lifting Nothing = Nothing

class SimpleParser a where
    simpleParser :: Parser a

class GetEncapsulatedParser t k where
    getEncapsulatedParser :: t -> Parser k

{-
-- ???
data Dom a = Dom

class ((k :-> p) t) => Parse p t k where
    parse :: Dom p -> BS.ByteString -> Maybe (BS.ByteString, t, Parser k)
-}

{-
-- FIXME: directed parsig: parse of encr -> parser of encd
instance (Parse a, Parse b) => Parse (a :? b) where
    parse str =     liftA (\ (bs, c) -> (bs, Inl c)) (parse str :: Maybe (BS.ByteString, a))
                <|> liftA (\ (bs, c) -> (bs, Inr c)) (parse str :: Maybe (BS.ByteString, b))

instance (Parse a, Parse b) => Parse (a :< b) where
    parse str = do
        (str', encr) <- parse str
        (str'', encd) <- parse str'
        return (str'', encr :< encd)
-}

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


-- | Complex encapsulating / decapsulatig
-- eprj : p1 :< p2 :< ... :< pn-1 :< pn :< ... :< pm ---> (p1 :< ... :< pn-1, pn :< ... :< pm)
--        ^ sup                                            ^ su         ^ sub
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
