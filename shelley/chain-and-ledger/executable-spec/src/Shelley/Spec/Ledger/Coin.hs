{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Shelley.Spec.Ledger.Coin
  ( module Shelley.Spec.Ledger.Data.CoinData,
    word64ToCoin,
    coinToRational,
    rationalToCoinViaFloor,
  )
where

import Shelley.Spec.Ledger.Data.CoinData
import Data.Word (Word64)

word64ToCoin :: Word64 -> Coin
word64ToCoin w = Coin $ fromIntegral w

coinToRational :: Coin -> Rational
coinToRational (Coin c) = fromIntegral c

rationalToCoinViaFloor :: Rational -> Coin
rationalToCoinViaFloor r = Coin . floor $ r
