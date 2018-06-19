{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}

{-| This module contains conversion operations that have the potential to fail.

    Such conversions are typically used when processing messages received from
    a DocuSign server instance, which may return values that are incomplete.

    Such messages typically include record instances for which only a subset of
    values are present. The precise subset of values included often depends on
    which particular operation was invoked, or on the current server state.

    The knowledge of when to expect that values will be present (and when to
    expect that they will be missing) is encapsulated in this module.
-}

module DocuSign.Client.Types.Parsing where

import DocuSign.Client.Types

import Control.Exception              ( Exception )
import Control.Monad                  ( join )
import Control.Monad.Catch            ( MonadThrow
                                      , throwM )
import Type.Reflection                ( TypeRep
                                      , typeRep )
import Data.Text                      ( Text )
import Data.Text.Read                 ( decimal )
import Data.Typeable                  ( Typeable )
import Data.UUID                      ( UUID )

import qualified Data.Text               as T
import qualified Data.UUID               as U
import qualified DocuSign.Base.Types as D

-- | A class for conversions that have the potential to fail.
class Parse a b where
  parse :: a -> Maybe b

-- | Parse a value monadically.
parseM :: forall a b m .
  MonadThrow m   =>
  Parse      a b =>
  Show       a   =>
  Typeable   a   =>
  Typeable   b   => a -> m b
parseM a = maybe (throwM (parseFailure a :: ParseFailure a b)) pure $ parse a

-- | Thrown when the base API fails to return a value that was expected.
data ParseFailure a b = ParseFailure
  { sourceValue :: a
  , sourceType  :: TypeRep a
  , targetType  :: TypeRep b }
  deriving Show

parseFailure :: Typeable a => Typeable b => a -> ParseFailure a b
parseFailure a = ParseFailure a typeRep typeRep

instance (Show a, Typeable a, Typeable b) => Exception (ParseFailure a b)

instance Parse D.Authentication [AccountInfo] where
  parse D.Authentication {..} =
    join $ fmap (traverse parse) authenticationLoginAccounts

instance Parse Text AccountId where
  parse = either (const Nothing) (Just . fst) . decimal

instance Parse D.LoginAccount AccountInfo where
  parse D.LoginAccount {..} =
    AccountInfo
      <$> (parse         =<< loginAccountAccountId)
      <*> (parse         =<< loginAccountIsDefault)
      <*> (mkAccountName <$> loginAccountName     )
      <*> (mkUserId      <$> loginAccountUserId   )
      <*> (mkUserName    <$> loginAccountUserName )

instance Parse Text UUID where
  parse = U.fromText

instance Parse Text EnvelopeId where
  parse = fmap mkEnvelopeId . parse

instance Parse D.EnvelopeSummary EnvelopeId where
  parse D.EnvelopeSummary {..} =
    parse =<< envelopeSummaryEnvelopeId

instance Parse D.EnvelopeViews Uri where
  parse D.EnvelopeViews {..} =
    mkUri <$> envelopeViewsUrl

instance Parse Text Bool where
  parse t = case T.unpack $ T.toLower t of
    't' : _ -> Just True
    'f' : _ -> Just False
    _       -> Nothing

instance Parse Text SigningEvent where
  parse = \case
    "cancel"            -> Just SigningCancelled
    "decline"           -> Just SigningDeclined
    "exception"         -> Just SigningException
    "fax_pending"       -> Just SigningFaxPending
    "id_check_failed"   -> Just SigningIdCheckFailed
    "session_timeout"   -> Just SigningSessionExpired
    "signing_complete"  -> Just SigningCompleted
    "ttl_expired"       -> Just SigningTokenExpired
    "viewing_complete"  -> Just SigningViewingCompleted
    _                   -> Nothing

