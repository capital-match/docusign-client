{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A simple client for communicating with a DocuSign server instance.

module DocuSign.Client
  ( DocuSignClient (..)
  , docuSignClient
  , runClient
  ) where

import DocuSign.Base.ContentTypes
import DocuSign.Client.Authentication
import DocuSign.Client.Types
import DocuSign.Client.Types.Conversion
import DocuSign.Client.Types.Parsing

import Control.Monad.IO.Class         ( liftIO )
import Data.Aeson                     ( encode )
import Data.ByteString                ( ByteString )
import Data.Default                   ( def )
import Data.List                      ( nub )
import Data.Text.Encoding             ( decodeUtf8 )
import Network.HTTP.Client            ( Request
                                      , managerModifyRequest
                                      , newManager
                                      , requestHeaders )
import Network.HTTP.Client.TLS        ( tlsManagerSettings )
import Network.HTTP.Types.Header      ( HeaderName )
import Servant.Client                 ( BaseUrl (..)
                                      , mkClientEnv
                                      , ClientM
                                      , Scheme (..)
                                      , runClientM )
import Servant.Client.Core.ClientError ( ClientError )

import qualified Data.ByteString.Base64            as Base64
import qualified Data.ByteString.Lazy              as BL
import qualified Data.Text                         as T
import qualified DocuSign.Base                 as D
import qualified DocuSign.Base.Types           as D
import qualified DocuSign.Client.Configuration as C

data DocuSignClient m = DocuSignClient {

    -- | List all accounts associated with the current set of credentials.
    listAccounts :: m [AccountInfo]

    -- | Post a set of documents to a single recipient for signing via email.
  , postDocumentsForEmailBasedSigning
      :: AccountId -> [Document] -> Envelope -> Recipient
      -> m EnvelopeId

    -- | Post a set of documents to a single recipient for signing via browser
    --   redirection.
  , postDocumentsForRedirectionBasedSigning
      :: AccountId -> [Document] -> Envelope -> Recipient
      -> (EnvelopeId -> PostSigningUri)
      -> m (EnvelopeId, SigningUri)

    -- | Fetch a document in its current state, regardless of whether it has
    --   or has not been signed.
  , fetchDocument :: AccountId -> EnvelopeId -> DocumentId -> m PDF

    -- | Generate a signing link for an existing envelope id
  , generateSigningLink :: AccountId -> EnvelopeId -> Recipient -> PostSigningUri -> m SigningUri
  }

docuSignClient :: DocuSignClient ClientM
docuSignClient = DocuSignClient

  { listAccounts =
      parseM =<< loginInformationGetLoginInformation
        Nothing Nothing Nothing Nothing

  , postDocumentsForEmailBasedSigning = postDocuments EmailBasedSigning

  , postDocumentsForRedirectionBasedSigning = \aid ds e r p -> do
      eid <- postDocuments RedirectionBasedSigning aid ds e r
      url <- generateSigningLink' aid eid (recipientClientUserId r) r (RedirectionOptions $ p eid)
      pure (eid, url)

  , fetchDocument = \aid eid did ->
      documentsGetDocument (convert aid) (convert eid) (convert did)
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing

  , generateSigningLink = \aid eid r p ->
      generateSigningLink' aid eid (recipientClientUserId r) r (RedirectionOptions p)
  }
  where
    D.DocuSignClient {..} = D.docuSignClient

    postDocuments sm aid ds Envelope {..} Recipient {..} =
      parseM =<< envelopesPostEnvelopes (convert aid) Nothing Nothing Nothing def
        { D.envelopeDefinitionEmailSubject = Just envelopeSubject
        , D.envelopeDefinitionEmailBlurb   = Just envelopeMessage
        , D.envelopeDefinitionStatus       = Just "sent"
        , D.envelopeDefinitionDocuments    = Just $ flip fmap ds $ \Document {..} ->
            def { D.documentDocumentBase64 = Just $ decodeUtf8 $ Base64.encode
                                                  $ toBytes documentContent
                , D.documentDocumentId     = Just $ convert documentId
                , D.documentName           = Just documentName }
        , D.envelopeDefinitionRecipients   = Just def
          { D.envelopeRecipientsSigners    = Just
            [ def { D.signerClientUserId   = case sm of
                                                  EmailBasedSigning -> Nothing
                                                  RedirectionBasedSigning ->
                                                    Just $ unUserId recipientClientUserId
                  , D.signerEmail          = Just $ unEmailAddress recipientEmailAddress
                  , D.signerName           = Just recipientName
                  , D.signerRecipientId    = Just "1"
                  , D.signerRoutingOrder   = Just "1"
                  , D.signerTabs           = flip fmap recipientSignatureAnchorText $ \a ->
                                                  def { D.envelopeRecipientTabsSignHereTabs = Just [
                                                  def { D.signHereAnchorString = Just a } ] } } ] } }

    generateSigningLink' aid eid uid Recipient {..} RedirectionOptions {..} =
      parseM =<< viewsPostEnvelopeRecipientView (convert aid) (convert eid) def
        { D.recipientViewRequestAuthenticationMethod = Just "email"
        , D.recipientViewRequestClientUserId         = Just $ unUserId uid
        , D.recipientViewRequestEmail                = Just $ unEmailAddress recipientEmailAddress
        , D.recipientViewRequestRecipientId          = Just "1"
        , D.recipientViewRequestReturnUrl            = Just $ unUri postSigningRedirectionUri
        , D.recipientViewRequestUserName             = Just recipientName }

runClient :: C.Config -> ClientM a -> IO (Either ClientError a)
runClient config client = do
    m <- liftIO $ newManager (tlsManagerSettings {managerModifyRequest = addHeaders})
    runClientM client $ mkClientEnv m (baseUrlFromConfig config)

  where

    addHeaders :: Request -> IO Request
    addHeaders r = pure $ r { requestHeaders = nub (authenticationHeader : requestHeaders r)}

    authenticationHeader :: (HeaderName, ByteString)
    authenticationHeader =
      ( authenticationHeaderKey
      , BL.toStrict $ encode $ authenticationHeaderFromConfig config)

    authenticationHeaderFromConfig :: C.Config -> AuthenticationHeader
    authenticationHeaderFromConfig (C.Config (C.AccountConfig _ k u p) _) =
      AuthenticationHeader k u p

    baseUrlFromConfig :: C.Config -> BaseUrl
    baseUrlFromConfig (C.Config _ s) =
      BaseUrl Https (T.unpack $ C.serverHost s) (C.serverPort s) "/restapi"

