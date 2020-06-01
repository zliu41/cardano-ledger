{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.Orphans where

import Cardano.Prelude (NoUnexpectedThunks)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.IP (IPv4, IPv6)
import qualified Data.Text.Encoding as Text

instance NoUnexpectedThunks IPv4

instance NoUnexpectedThunks IPv6

instance ToJSON ByteString where
  toJSON = String . Text.decodeUtf8 . Base16.encode

instance FromJSON ByteString where
  parseJSON = withText "FromJSON ByteString" $ \ t ->
    case Base16.decode $ Text.encodeUtf8 t of
      (res, "") -> pure res
      (_, bs) -> fail $ "FromJSON ByteString: Failed to decode " ++ show bs
