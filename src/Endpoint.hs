{-# LANGUAGE DeriveGeneric 
  , OverloadedStrings #-}
module Endpoint where

import System.Environment
import GHC.Generics
import Data.Char (toLower)
import Data.String (fromString)

import Data.Aeson (Value, decode, ToJSON, FromJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Web.Scotty
import Control.Lens
import Network.Wreq (Response)
import qualified Network.Wreq as W

data Endpoint = Endpoint
    { endpoint :: String
    , method :: Text
    , value :: Value -- JSON
    } deriving (Generic, Show)

instance ToJSON Endpoint
instance FromJSON Endpoint

readFiles :: [FilePath] -> IO [ByteString]
readFiles = mapM B.readFile

parse :: [ByteString] -> Maybe [Endpoint]
parse = sequence . map decode

createServer :: [Endpoint] -> ScottyM [()]
createServer = mapM create
    where
        create (Endpoint ep m v) =
            case T.toLower m of
                "get" -> get (fromString ep) $ do json v
                _ -> notFound $ do text "nope"

req :: Int -> Endpoint -> IO Bool
req port (Endpoint ep m v) = do
    let url = "http://localhost:" ++ show port
    r <- W.asJSON =<< W.get url :: IO (Response Value)
    return $ r ^. W.responseBody == v

testAPI :: Int -> [Endpoint] -> IO [Bool]
testAPI port = sequence . map (req port)
