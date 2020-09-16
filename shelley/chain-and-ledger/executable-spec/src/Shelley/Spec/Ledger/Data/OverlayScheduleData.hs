{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Shelley.Spec.Ledger.Data.OverlayScheduleData
  ( -- * OBftSlot
    OBftSlot (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    TokenType (TypeNull),
    decodeNull,
    encodeNull,
    peekTokenType,
  )
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NFData, NoUnexpectedThunks)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Data.KeysData
  ( KeyHash (..),
    KeyRole (..),
  )

data OBftSlot era
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis era)
  deriving (Show, Eq, Ord, Generic)

instance
  Era era =>
  ToCBOR (OBftSlot era)
  where
  toCBOR NonActiveSlot = encodeNull
  toCBOR (ActiveSlot k) = toCBOR k

instance
  Era era =>
  FromCBOR (OBftSlot era)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> fromCBOR

instance NoUnexpectedThunks (OBftSlot era)

instance NFData (OBftSlot era)
