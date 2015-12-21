{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Config
    ( readConfig
    , Config(..)
    , Network(..)
    , Channel(..)) where

import Control.Applicative ((<$>), (<*>), pure, (<|>))
import Data.Aeson (Value (..), FromJSON (..), (.:), (.:?), Object, eitherDecode)
import Data.Text (Text)

import GHC.Generics (Generic) -- used to automatically create instances of Hashable
import Data.Hashable -- (Hashable)

import System.IO (withFile, IOMode(ReadMode), hGetContents)

--import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as B


data Config = Config
    { configNetworks :: [Network]
    , configAuthToken :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON Config where
    parseJSON (Object v) =
        Config <$> v .: "networks"
               <*> v .: "auth_token" -- for GitHub
    parseJSON _ = fail "Config must be an object"

instance Hashable Config

data Network = Network
    { networkName :: Text
    , networkServer :: Text
    , networkPort :: Int
    , networkNick :: Text
    , networkChannels :: [Channel]
    } deriving (Show, Eq, Generic)

instance FromJSON Network where
    parseJSON (Object v) =
        Network <$> v .: "name"
                <*> v .: "server"
                <*> v .: "port"
                <*> v .: "nick"
                <*> v .: "channels"
    parseJSON _ = fail "Network must be an object"

instance Hashable Network

data Channel = Channel {
    channelName :: Text
    , channelWatchedGithubRepoOwner :: Maybe Text -- owner of the watched repo
    , channelWatchedGithubRepo :: Maybe Text
    , channelEnableOffensiveLanguage :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON Channel where
    parseJSON (Object v) =
        Channel <$> v .: "name"
                <*> v .:? "watched_repo_owner" -- TODO: Store this in a subobject instead, so that multiple repos can be watched!
                <*> v .:? "watched_repo"
                <*> v .: "offensive_language"
    parseJSON _ = fail "Channel must be an object"

instance Hashable Channel

-- Mapping filename to IO action returning a Config object
readConfig :: String -> IO (Maybe Config)
readConfig filename = do
                        json <- readFile filename
                        dec <- return ((eitherDecode $ B.pack json) :: (Either String Config)) -- TODO: Clean this up!
                        case dec of
                            Left err -> putStrLn err >> return Nothing
                            Right config -> return $ Just config

