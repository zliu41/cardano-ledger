{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.Data.AddressData
  ( Addr (..),
    BootstrapAddress (..),
    RewardAcnt (..),
    Word7 (..),
  )
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Ledger.Era
import Cardano.Prelude (NFData, NoUnexpectedThunks)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Binary (Word8)
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.Data.BaseTypesData (Network (..))
import Shelley.Spec.Ledger.Data.CredentialData
  ( Credential (..),
    PaymentCredential,
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Data.KeysData
  ( KeyRole (..),
  )

-- | An address for UTxO.
data Addr era
  = Addr !Network !(PaymentCredential era) !(StakeReference era)
  | AddrBootstrap !(BootstrapAddress era)
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoUnexpectedThunks (Addr era)

-- | An account based address for rewards
data RewardAcnt era = RewardAcnt
  { getRwdNetwork :: !Network,
    getRwdCred :: !(Credential 'Staking era)
  }
  deriving (Show, Eq, Generic, Ord, NFData, ToJSONKey, FromJSONKey)

instance Era era => ToJSON (RewardAcnt era) where
  toJSON ra =
    Aeson.object
      [ "network" .= getRwdNetwork ra,
        "credential" .= getRwdCred ra
      ]

instance Era era => FromJSON (RewardAcnt era) where
  parseJSON =
    Aeson.withObject "RewardAcnt" $ \obj ->
      RewardAcnt
        <$> obj .: "network"
        <*> obj .: "credential"

instance NoUnexpectedThunks (RewardAcnt era)

newtype Word7 = Word7 Word8
  deriving (Eq, Show)

newtype BootstrapAddress era = BootstrapAddress
  { unBootstrapAddress :: Byron.Address
  }
  deriving (Eq, Generic)
  deriving newtype (NFData, Ord)
  deriving (Show) via Quiet (BootstrapAddress era)

instance NoUnexpectedThunks (BootstrapAddress era)
