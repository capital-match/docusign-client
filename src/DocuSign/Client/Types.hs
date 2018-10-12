{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DocuSign.Client.Types
  ( AccountId
  , AccountName
  , DocumentId
  , EmailAddress
  , EnvelopeId
  , PostSigningUri
  , RecipientId
  , SigningUri
  , Uri
  , UserId
  , UserName

  , mkAccountId
  , mkAccountName
  , mkDocumentId
  , mkEmailAddress
  , mkEnvelopeId
  , mkRecipientId
  , mkUri
  , mkUserId
  , mkUserName

  , unAccountId
  , unAccountName
  , unDocumentId
  , unEmailAddress
  , unEnvelopeId
  , unRecipientId
  , unUri
  , unUserId
  , unUserName

  , AccountInfo (..)
  , Document (..)
  , Envelope (..)
  , Recipient (..)
  , RedirectionOptions (..)
  , SigningEvent (..)
  , SigningMethod (..)
  ) where

import Control.Exception              ( assert )
import Data.Aeson                     ( FromJSON
                                      , ToJSON )
import Data.Data                      ( Data )
import Data.Text                      ( Text )
import Data.Typeable                  ( Typeable )
import Data.UUID                      ( UUID )
import DocuSign.Base.ContentTypes ( PDF )

-- Type aliases.

-- | Represents a single-use signing link.
type SigningUri = Uri

-- | Represents a post-signing redirection target.
type PostSigningUri = Uri

-- Simple boxed value types.

newtype AccountId = AccountId { unAccountId :: Integer }
  deriving (Enum, Eq, FromJSON, Integral, Num, Ord, Read, Real, Show, ToJSON, Typeable)

mkAccountId :: Integer -> AccountId
mkAccountId i = assert (i > 0) $ AccountId i

newtype AccountName = AccountName { unAccountName :: Text }
  deriving (Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkAccountName :: Text -> AccountName
mkAccountName = AccountName

newtype DocumentId = DocumentId { unDocumentId :: Integer }
  deriving (Data, Enum, Eq, FromJSON, Integral, Num, Ord, Read, Real, Show, ToJSON, Typeable)

mkDocumentId :: Integer -> DocumentId
mkDocumentId i = assert (i > 0) $ DocumentId i

newtype EmailAddress = EmailAddress { unEmailAddress :: Text }
  deriving (Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkEmailAddress :: Text -> EmailAddress
mkEmailAddress = EmailAddress

newtype EnvelopeId = EnvelopeId { unEnvelopeId :: UUID }
  deriving (Data, Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkEnvelopeId :: UUID -> EnvelopeId
mkEnvelopeId = EnvelopeId

newtype RecipientId = RecipientId { unRecipientId :: Text }
  deriving (Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkRecipientId :: Text -> RecipientId
mkRecipientId = RecipientId

newtype Uri = Uri { unUri :: Text }
  deriving (Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkUri :: Text -> Uri
mkUri = Uri

newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkUserId :: Text -> UserId
mkUserId = UserId

newtype UserName = UserName { unUserName :: Text }
  deriving (Eq, FromJSON, Read, Show, ToJSON, Typeable)

mkUserName :: Text -> UserName
mkUserName = UserName

-- Sum types.

data SigningEvent
  = SigningCancelled
  | SigningCompleted
  | SigningDeclined
  | SigningException
  | SigningFaxPending
  | SigningIdCheckFailed
  | SigningSessionExpired
  | SigningTokenExpired
  | SigningViewingCompleted
  deriving (Eq, Show, Typeable)

data SigningMethod
  = EmailBasedSigning
  | RedirectionBasedSigning

-- Record types.

data AccountInfo = AccountInfo
  { accountId        :: AccountId
  , accountIsDefault :: Bool
  , accountName      :: AccountName
  , accountUserId    :: UserId
  , accountUserName  :: UserName }

data Document = Document
  { documentContent :: PDF
  , documentId      :: DocumentId
  , documentName    :: Text }

data Envelope = Envelope
  { envelopeSubject :: Text
  , envelopeMessage :: Text }

data Recipient = Recipient
  { recipientClientUserId        :: UserId
  , recipientEmailAddress        :: EmailAddress
  , recipientName                :: Text
  , recipientSignatureAnchorText :: Maybe Text }

newtype RedirectionOptions = RedirectionOptions
  { postSigningRedirectionUri :: Uri }

