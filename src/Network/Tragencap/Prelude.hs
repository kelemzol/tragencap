
module Network.Tragencap.Prelude where

import qualified Data.ByteString.Lazy as BS

import Network.Tragencap.Core


-- * Port
---------------------------------------

newtype Port
  = Port
    { port :: Int }
  deriving (Eq, Ord, Show)

-- * Payload
---------------------------------------

newtype Payload
  = Payload
    { payload :: BS.ByteString }
  deriving (Eq, Ord, Show)

payloadParser :: Parser Payload
payloadParser = Parser (\ bs -> Just (BS.pack [], Payload bs))

instance SimpleParser Payload where
	simpleParser = payloadParser
