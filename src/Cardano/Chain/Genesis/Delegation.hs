{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cardano.Chain.Genesis.Delegation
  ( GenesisDelegation(..)
  , mkGenesisDelegation
  )
where

import Cardano.Prelude

import Control.Lens (at, (^.))
import Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson
import Data.List (nub)
import qualified Data.Map.Strict as M
import Formatting (build, sformat)
import Text.JSON.Canonical (FromJSON(..), ReportSchemaErrors(..), ToJSON(..))

import Cardano.Chain.Common (StakeholderId(..), addressHash)
import Cardano.Chain.Delegation.HeavyDlgIndex (ProxySKHeavy)
import Cardano.Crypto (ProxySecretKey(..), isSelfSignedPsk)


-- | This type contains genesis state of heavyweight delegation. It wraps a map
--   where keys are issuers (i. e. stakeholders who delegated) and values are
--   proxy signing keys. There are some invariants:
--
--   1. In each pair delegate must differ from issuer, i. e. no revocations.
--   2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--      equal to the key in the map.
--   3. Delegates can't be issuers, i. e. transitive delegation is not
--      supported. It's not needed in genesis, it can always be reduced.
--
newtype GenesisDelegation = UnsafeGenesisDelegation
  { unGenesisDelegation :: Map StakeholderId ProxySKHeavy
  } deriving (Show, Eq)

instance Monad m => ToJSON m GenesisDelegation where
  toJSON = toJSON . unGenesisDelegation

instance MonadError SchemaError m => FromJSON m GenesisDelegation where
  fromJSON val = do
    psks <- fromJSON val
    case recreateGenesisDelegation psks of
      Left err -> expected "GenesisDelegation" (Just $ "Error: " <> toS err)
      Right delegation -> pure delegation

instance Aeson.ToJSON GenesisDelegation where
  toJSON = Aeson.toJSON . unGenesisDelegation

instance Aeson.FromJSON GenesisDelegation where
  parseJSON = Aeson.parseJSON >=> \v -> do
    (elems' :: Map StakeholderId ProxySKHeavy) <- mapM Aeson.parseJSON v
    toAesonError $ recreateGenesisDelegation elems'

-- | Safe constructor of 'GenesisDelegation' from a list of PSKs.
mkGenesisDelegation
  :: MonadError Text m => [ProxySKHeavy] -> m GenesisDelegation
mkGenesisDelegation psks = do
  unless ((length . nub $ pskIssuerPk <$> psks) == length psks)
    $ throwError "all issuers must be distinct"
  let
    res = M.fromList
      [ (StakeholderId $ addressHash (pskIssuerPk psk), psk) | psk <- psks ]
  recreateGenesisDelegation res

-- | Safe constructor of 'GenesisDelegation' from existing map.
recreateGenesisDelegation
  :: MonadError Text m => Map StakeholderId ProxySKHeavy -> m GenesisDelegation
recreateGenesisDelegation pskMap = do
  forM_ (M.toList pskMap) $ \(k, psk) ->
    when ((StakeholderId $ addressHash (pskIssuerPk psk)) /= k)
      $ throwError
      $ sformat
          ( "wrong issuerPk set as key for delegation map: "
          . "issuer id = "
          . build
          . ", cert id = "
          . build
          )
          k
          (addressHash (pskIssuerPk psk))
  when (any isSelfSignedPsk pskMap)
    $ throwError "there is a self-signed (revocation) psk"
  let
    isIssuer psk =
      isJust $ pskMap ^. at (StakeholderId $ addressHash (pskDelegatePk psk))

  when (any isIssuer pskMap)
    $ throwError "one of the delegates is also an issuer, don't do it"
  return $ UnsafeGenesisDelegation pskMap
