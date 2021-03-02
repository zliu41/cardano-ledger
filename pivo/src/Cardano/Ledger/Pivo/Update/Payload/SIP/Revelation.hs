{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP.Revelation where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)

import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)
import Cardano.Crypto.DSIGN (hashVerKeyDSIGN)

import Cardano.Ledger.Era (Era)

import Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal (Proposal)
import Cardano.Ledger.Pivo.Update.Payload.Types (VKeyHash, VKey)

data Revelation era
  = Revelation
    { proposal  :: Proposal era
      -- ^ Proposal that is being revealed.
    , revelator :: VKeyHash era
      -- ^ Revelation author. This should coincide with the submission author.
    , salt      :: Int
      -- ^ Salt used to calculate the commit.
    } deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON, FromJSON)

instance (Typeable era, Era era) => ToCBOR (Revelation era) where
  toCBOR Revelation { proposal, revelator, salt }
    =  encodeListLen 3
    <> toCBOR proposal
    <> toCBOR revelator
    <> toCBOR salt

instance (Typeable era, Era era) => FromCBOR (Revelation era) where
  fromCBOR = do
    decodeListLenOf 3
    p <- fromCBOR
    r <- fromCBOR
    s <- fromCBOR
    return $! Revelation p r s

mkRevelation
  :: Era era
  => VKey era
  -> Int
  -> Proposal era
  -> Revelation era
mkRevelation vk someSalt someProposal =
  Revelation
    { proposal  = someProposal
    , revelator = hashVerKeyDSIGN vk
    , salt      = someSalt
    }
