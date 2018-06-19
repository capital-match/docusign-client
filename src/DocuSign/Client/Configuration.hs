{-# LANGUAGE DeriveGeneric #-}

module DocuSign.Client.Configuration
  ( Config (..)
  , AccountConfig (..)
  , ServerConfig (..)
  ) where

import Data.Text    ( Text )
import Data.UUID    ( UUID )
import GHC.Generics ( Generic )

import DocuSign.Client.Types
import DocuSign.Base.Fields

import qualified Data.Aeson as J

data Config = Config
  { configAccount :: AccountConfig
  , configServer  :: ServerConfig
  } deriving (Generic, Read, Show)

instance J.FromJSON Config where
  parseJSON = J.genericParseJSON (removeFieldLabelPrefix "config")
instance J.ToJSON Config where
  toJSON = J.genericToJSON (removeFieldLabelPrefix "config")

data AccountConfig = AccountConfig
  { accountId       :: AccountId
  , accountKey      :: UUID
  , accountUsername :: Text
  , accountPassword :: Text
  } deriving (Generic, Read, Show)

instance J.FromJSON AccountConfig where
  parseJSON = J.genericParseJSON (removeFieldLabelPrefix "account")
instance J.ToJSON AccountConfig where
  toJSON = J.genericToJSON (removeFieldLabelPrefix "account")

data ServerConfig = ServerConfig
  { serverHost :: Text
  , serverPort :: Int
  } deriving (Generic, Read, Show)

instance J.FromJSON ServerConfig where
  parseJSON = J.genericParseJSON (removeFieldLabelPrefix "server")
instance J.ToJSON ServerConfig where
  toJSON = J.genericToJSON (removeFieldLabelPrefix "server")

