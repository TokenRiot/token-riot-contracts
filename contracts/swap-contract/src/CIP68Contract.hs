{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module CIP68Contract
  ( cip68ContractScript
  , ScriptParameters(..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley    ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as V2
import           OptimizerOptions       ( theOptimizerOptions )
import           Plutonomy
import           SwappableDataType
import           ReferenceDataType
import           UsefulFuncs
import           ReducedFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Starter NFT Contract Parameterization
-------------------------------------------------------------------------------
data ScriptParameters = ScriptParameters
  { lockPid :: V2.CurrencySymbol
  -- ^ The locking token's policy id.
  , lockTkn :: V2.TokenName
  -- ^ The locking token's token name
  , refHash :: V2.ValidatorHash
  -- ^ The validator hash of the data reference contract
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { metadata :: V2.Map BuiltinData BuiltinData
  -- ^ The map of the metadata fields
  , version  :: Integer
  -- ^ The metadata version number
  }
PlutusTx.makeIsDataIndexed ''CustomDatumType  [('CustomDatumType, 0)]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove | Update ADAIncData
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [('Remove, 0 ), ('Update, 1 )]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ScriptParameters -> CustomDatumType -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator ScriptParameters {..} _ redeemer context =
  case redeemer of
    -- Allows the reference data hot key to remove entries
    Remove ->
      let !info                 = V2.scriptContextTxInfo context
          !txInputs             = V2.txInfoInputs info
          !txOutputs            = V2.txInfoOutputs info
          !txSigners            = V2.txInfoSignatories info
          !validatingInput      = ownInput context
          !scriptAddr           = V2.txOutAddress validatingInput
          !contTxOutputs        = getScriptOutputs txOutputs scriptAddr
          !refTxIns             = V2.txInfoReferenceInputs info
          !refTxOut             = getReferenceInput refTxIns refHash
          !(Reference _ _ sd _) = getReferenceDatum refTxOut
          !refValue             = V2.txOutValue refTxOut
          !hotPkh               = mHot sd
          !lockValue            = Value.singleton lockPid lockTkn (1 :: Integer)
      in traceIfFalse "ins" (nInputs txInputs scriptAddr 1)  -- single tx going in
      && traceIfFalse "sig" (signedBy txSigners hotPkh)      -- hot key must sign
      && traceIfFalse "out" (nOutputs contTxOutputs 0)       -- nothing going out
      && traceIfFalse "val" (Value.geq refValue lockValue)   -- check if correct reference

    -- Allows the reference data hot key to update entries
    (Update aid) ->
      let !info                 = V2.scriptContextTxInfo context
          !txInputs             = V2.txInfoInputs info
          !txOutputs            = V2.txInfoOutputs info
          !txSigners            = V2.txInfoSignatories info
          !validatingInput      = ownInput context
          !thisValue            = V2.txOutValue validatingInput
          !scriptAddr           = V2.txOutAddress validatingInput
          !contTxOutputs        = getScriptOutputs txOutputs scriptAddr
          !refTxIns             = V2.txInfoReferenceInputs info
          !refTxOut             = getReferenceInput refTxIns refHash
          !(Reference _ _ sd _) = getReferenceDatum refTxOut
          !refValue             = V2.txOutValue refTxOut
          !hotPkh               = mHot sd
          !lockValue            = Value.singleton lockPid lockTkn (1 :: Integer)
          !incomingValue        = thisValue + adaValue (adaInc aid)
      in traceIfFalse "ins" (nInputs txInputs scriptAddr 1)           -- single tx going in
      && traceIfFalse "sig" (signedBy txSigners hotPkh)               -- hot key must sign
      && traceIfFalse "out" (nOutputs contTxOutputs 1)                -- single going out
      && traceIfFalse "val" (Value.geq refValue lockValue)            -- check if correct reference
      && traceIfFalse "dat" (isValueCont contTxOutputs incomingValue) -- check if value is continue by datum
--Functions--------------------------------------------------------------------
  where
    getReferenceDatum :: V2.TxOut -> ReferenceDatum
    getReferenceDatum x = 
      case V2.txOutDatum x of
        V2.NoOutputDatum              -> traceError "No Datum"
        (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @ReferenceDatum inline

    isValueCont :: [V2.TxOut] -> V2.Value -> Bool
    isValueCont txOuts val' = isValueCont' txOuts val'
      where
        isValueCont' :: [V2.TxOut] -> V2.Value -> Bool
        isValueCont' []     _   = traceError "Nothing Found"
        isValueCont' (x:xs) val =
          if V2.txOutValue x == val -- strict value continue
            then
              case V2.txOutDatum x of
                V2.NoOutputDatum              -> isValueCont' xs val -- skip datumless
                (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
                (V2.OutputDatum (V2.Datum d)) -> 
                  case PlutusTx.fromBuiltinData @CustomDatumType d of
                    Nothing -> traceError "Bad Data"
                    Just _  -> True
            else isValueCont' xs val
    
-------------------------------------------------------------------------------
-- | Now we need to compile the validator.
-------------------------------------------------------------------------------
wrappedValidator :: ScriptParameters -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator s x y z = check (mkValidator s (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y) (V2.unsafeFromBuiltinData z))

validator :: ScriptParameters -> V2.Validator
validator sp = Plutonomy.optimizeUPLCWith theOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
  $$(PlutusTx.compile [|| wrappedValidator ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

cip68ContractScript :: ScriptParameters -> PlutusScript PlutusScriptV2
cip68ContractScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . validator