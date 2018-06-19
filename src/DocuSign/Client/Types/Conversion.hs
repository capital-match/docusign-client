{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-| This module contains conversion operations that are guaranteed not to fail.

    Such conversions are typically used when preparing messages to be sent to a
    DocuSign server instance, and convert from types exposed by the user-facing
    high-level client API to types consumed by the lower-level REST API.
-}

module DocuSign.Client.Types.Conversion where

import DocuSign.Client.Types

import Data.Text ( Text )

import qualified Data.Text as T
import qualified Data.UUID as U

-- | A class for conversions that are guaranteed not to fail.
class Convert a b where
  convert :: a -> b

instance Convert AccountId Text where
  convert = T.pack . show . unAccountId

instance Convert DocumentId Text where
  convert = T.pack . show . unDocumentId

instance Convert EnvelopeId Text where
  convert = U.toText . unEnvelopeId

