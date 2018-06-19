{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DocuSign.Client.Authentication
  ( AuthenticationHeader (..)
  , authenticationHeaderKey
  ) where

import Data.Aeson                 ( FromJSON
                                  , ToJSON
                                  , defaultOptions
                                  , genericParseJSON
                                  , genericToJSON
                                  , parseJSON
                                  , toJSON )
import Data.UUID                  ( UUID )
import Data.Text                  ( Text )
import DocuSign.Base.Fields   ( modifyFieldLabel )
import GHC.Generics               ( Generic )
import Network.HTTP.Types.Header  ( HeaderName )

data AuthenticationHeader = AuthenticationHeader
  { authenticationHeaderIntegratorKey :: UUID
  , authenticationHeaderUsername      :: Text
  , authenticationHeaderPassword      :: Text
  } deriving (Show, Eq, Generic)

-- In the JSON serialization of an AuthenticationHeader, field names must start
-- with a capital letter, which is against the normal convention. Hence we must
-- define custom implementations of FromJSON and ToJSON:

instance FromJSON AuthenticationHeader where
  parseJSON = genericParseJSON
    $ modifyFieldLabel (dropLength "authenticationHeader")
    $ defaultOptions

instance ToJSON AuthenticationHeader where
  toJSON = genericToJSON
    $ modifyFieldLabel (dropLength "authenticationHeader")
    $ defaultOptions

authenticationHeaderKey :: HeaderName
authenticationHeaderKey = "X-DocuSign-Authentication"

dropLength :: String -> String -> String
dropLength = drop . length

