{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era,
    Crypto,
    ValueType,
    TxBody,
  )
where

import Cardano.Prelude( NoUnexpectedThunks (..) )
import Cardano.Binary( FromCBOR, ToCBOR, Annotator)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)
-- import Shelley.Spec.Ledger.Coin(Coin)
import Shelley.Spec.Ledger.Val as ValClass
-- import Data.Set(Set)
-- import Data.Sequence.Strict (StrictSeq)
{-
import Shelley.Spec.Ledger.TxData
  ( TxIn,
    TxOut,
    DCert,
    Wdrl,
  )
-}


class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e,
    Typeable (TxBody e), Show(TxBody e), Eq(TxBody e), NoUnexpectedThunks(TxBody e),
      ToCBOR(TxBody e), FromCBOR(TxBody e), FromCBOR(Annotator (TxBody e)),

    ValClass.Val (ValueType e)  -- Multi Assets
  ) =>
  Era e
  where
  type Crypto e :: Type
  type ValueType e :: Type
  type TxBody e :: Type


{-
class Body body where
  _inputs' :: Era e => body -> (Set (TxIn e))
  _outputs' :: Era e => body -> (StrictSeq (TxOut e))
  _certs' :: Era e => body -> (StrictSeq (DCert e))
  _wdrls' :: Era e => body-> (Wdrl e)
  _txfee' :: body -> Coin
  _ttl' ::  body -> SlotNo
  _txUpdate' :: Era e => body -> (StrictMaybe (Update e))
  bodyBytes ::  body -> LByteString

-}