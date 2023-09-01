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
module ReferenceContract
  ( referenceContractScript
  ) where
import           Cardano.Api.Shelley   ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           OptimizerOptions      ( theOptimizerOptions )
import           Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api  as V2
import           ReducedFunctions
import           ReferenceDataType
import qualified UsefulFuncs           as UF
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType
  = UpdateCashier IncreaseData
  | UpdateFee IncreaseData
  | UpdateMultisig IncreaseData
  | UpdateHotKey IncreaseData
  | UpdatePool IncreaseData
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('UpdateCashier,  0)
                                                , ('UpdateFee,      1)
                                                , ('UpdateMultisig, 2)
                                                , ('UpdateHotKey,   3)
                                                , ('UpdatePool,     4)
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ReferenceDatum -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case (datum, redeemer) of
    -- | Update the cashier address
    (Reference _ sf sd sp, UpdateCashier aid) ->
      let !info                      = V2.scriptContextTxInfo context
          !txSigners                 = V2.txInfoSignatories info
          !txInputs                  = V2.txInfoInputs info
          !txOutputs                 = V2.txInfoOutputs info
          !validatingInput           = ownInput context
          !thisValue                 = V2.txOutValue validatingInput
          !incomingValue             = thisValue + UF.adaValue (idADA aid)
          !scriptAddr                = V2.txOutAddress validatingInput
          !contTxOutputs             = getScriptOutputs txOutputs scriptAddr
          !listOfPkh                 = mPkhs sd
          !threshold                 = mThres sd
          !(Reference _ sf' sd' sp') = getOutboundDatumByValue txOutputs incomingValue
      in traceIfFalse "sig" (checkMultisig txSigners listOfPkh threshold) -- valid multisig 
      && traceIfFalse "ins" (nInputs txInputs scriptAddr 1)               -- single tx going in
      && traceIfFalse "out" (nOutputs contTxOutputs 1)                    -- single going out
      && traceIfFalse "dat" (sd == sd')                                   -- signers cant change
      && traceIfFalse "ser" (sf == sf')                                   -- service fees cant change
      && traceIfFalse "sta" (sp == sp')                                   -- stake pool cant change
    
    -- | Update the service fee
    (Reference pd _ sd sp, UpdateFee aid) ->
      let !info                      = V2.scriptContextTxInfo context
          !txSigners                 = V2.txInfoSignatories info
          !txInputs                  = V2.txInfoInputs info
          !txOutputs                 = V2.txInfoOutputs info
          !validatingInput           = ownInput context
          !thisValue                 = V2.txOutValue validatingInput
          !incomingValue             = thisValue + UF.adaValue (idADA aid)
          !scriptAddr                = V2.txOutAddress validatingInput
          !contTxOutputs             = getScriptOutputs txOutputs scriptAddr
          !listOfPkh                 = mPkhs sd
          !threshold                 = mThres sd
          !(Reference pd' _ sd' sp') = getOutboundDatumByValue txOutputs incomingValue
      in traceIfFalse "sig" (checkMultisig txSigners listOfPkh threshold) -- valid multisig 
      && traceIfFalse "ins" (nInputs txInputs scriptAddr 1)               -- single tx going in
      && traceIfFalse "out" (nOutputs contTxOutputs 1)                    -- single going out
      && traceIfFalse "pay" (pd == pd')                                   -- payment data cant change
      && traceIfFalse "dat" (sd == sd')                                   -- signers cant change
      && traceIfFalse "sta" (sp == sp')                                   -- stake pool cant change

    -- | Update the multisig
    (Reference pd sf sd sp, UpdateMultisig aid) ->
      let !info                        = V2.scriptContextTxInfo context
          !txSigners                   = V2.txInfoSignatories info
          !txInputs                    = V2.txInfoInputs info
          !txOutputs                   = V2.txInfoOutputs info
          !validatingInput             = ownInput context
          !thisValue                   = V2.txOutValue validatingInput
          !incomingValue               = thisValue + UF.adaValue (idADA aid)
          !scriptAddr                  = V2.txOutAddress validatingInput
          !contTxOutputs               = getScriptOutputs txOutputs scriptAddr
          !listOfPkh                   = mPkhs sd
          !threshold                   = mThres sd
          !(Reference pd' sf' sd' sp') = getOutboundDatumByValue txOutputs incomingValue
      in traceIfFalse "sig" (checkMultisig txSigners listOfPkh threshold) -- valid multisig 
      && traceIfFalse "ins" (nInputs txInputs scriptAddr 1)               -- single tx going in
      && traceIfFalse "out" (nOutputs contTxOutputs 1)                    -- single going out
      && traceIfFalse "pay" (pd == pd')                                   -- payment data cant change
      && traceIfFalse "ser" (sf == sf')                                   -- service fees cant change
      && traceIfFalse "sta" (sp == sp')                                   -- stake pool cant change
      && traceIfFalse "mul" (lengthCheck sd')                             -- valid new multisig
      && traceIfFalse "hot" (mHot sd == mHot sd')                         -- hot key cant change
    
    -- | Update the hotkey
    (Reference pd sf sd sp, UpdateHotKey aid) ->
      let !info                        = V2.scriptContextTxInfo context
          !txSigners                   = V2.txInfoSignatories info
          !txInputs                    = V2.txInfoInputs info
          !txOutputs                   = V2.txInfoOutputs info
          !validatingInput             = ownInput context
          !thisValue                   = V2.txOutValue validatingInput
          !incomingValue               = thisValue + UF.adaValue (idADA aid)
          !scriptAddr                  = V2.txOutAddress validatingInput
          !contTxOutputs               = getScriptOutputs txOutputs scriptAddr
          !listOfPkh                   = mPkhs sd
          !threshold                   = mThres sd
          !(Reference pd' sf' sd' sp') = getOutboundDatumByValue txOutputs incomingValue
      in traceIfFalse "sig" (checkMultisig txSigners listOfPkh threshold) -- valid multisig 
      && traceIfFalse "ins" (nInputs txInputs scriptAddr 1)               -- single tx going in
      && traceIfFalse "out" (nOutputs contTxOutputs 1)                    -- single going out
      && traceIfFalse "pay" (pd == pd')                                   -- payment data cant change
      && traceIfFalse "ser" (sf == sf')                                   -- service fees cant change
      && traceIfFalse "sta" (sp == sp')                                   -- stake pool cant change
      && traceIfFalse "hot" (changeHotKeyOnly sd sd')                     -- hot key change only
    
    -- | Update stake pool info
    (Reference pd sf sd _, UpdatePool aid) ->
      let !info                      = V2.scriptContextTxInfo context
          !txSigners                 = V2.txInfoSignatories info
          !txInputs                  = V2.txInfoInputs info
          !txOutputs                 = V2.txInfoOutputs info
          !validatingInput           = ownInput context
          !thisValue                 = V2.txOutValue validatingInput
          !incomingValue             = thisValue + UF.adaValue (idADA aid)
          !scriptAddr                = V2.txOutAddress validatingInput
          !contTxOutputs             = getScriptOutputs txOutputs scriptAddr
          !listOfPkh                 = mPkhs sd
          !threshold                 = mThres sd
          !(Reference pd' sf' sd' _) = getOutboundDatumByValue txOutputs incomingValue
      in traceIfFalse "sig" (checkMultisig txSigners listOfPkh threshold) -- valid multisig 
      && traceIfFalse "ins" (nInputs txInputs scriptAddr 1)               -- single tx going in
      && traceIfFalse "out" (nOutputs contTxOutputs 1)                    -- single going out
      && traceIfFalse "pay" (pd == pd')                                   -- payment data cant change
      && traceIfFalse "sta" (sf == sf')                                   -- stake pool cant change
      && traceIfFalse "dat" (sd == sd')                                   -- signers cant change
      && traceIfFalse "ran" (rs == rs)
      -- ^ change rs then we can compile a new contract w/o parameterization.
    
  where
    -- | A random string to create a new contract.
    rs :: [Integer]
    rs = [0,1]

    -- | get the datum by searching the tx outputs by the validating value
    getOutboundDatumByValue :: [V2.TxOut] -> V2.Value -> ReferenceDatum
    getOutboundDatumByValue txOuts val' = getOutboundDatumByValue' txOuts val'
      where
        getOutboundDatumByValue' :: [V2.TxOut] -> V2.Value -> ReferenceDatum
        getOutboundDatumByValue' []     _    = traceError "No Datum"
        getOutboundDatumByValue' (x:xs) !val =
          if V2.txOutValue x == val                                            -- strict value continue
          then
            case V2.txOutDatum x of
              V2.NoOutputDatum              -> getOutboundDatumByValue' xs val -- skip datumless
              (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
              (V2.OutputDatum (V2.Datum d)) ->                                 -- inline datum only
                case PlutusTx.fromBuiltinData d of
                  Nothing     -> traceError "Bad Datum"
                  Just inline -> PlutusTx.unsafeFromBuiltinData @ReferenceDatum inline
          else getOutboundDatumByValue' xs val
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator x y z = check (mkValidator (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y) (V2.unsafeFromBuiltinData z))

validator :: V2.Validator
validator = Plutonomy.optimizeUPLCWith theOptimizerOptions $ 
  Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
  $$(PlutusTx.compile [|| wrappedValidator ||])

referenceContractScriptShortBs :: SBS.ShortByteString
referenceContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

referenceContractScript :: PlutusScript PlutusScriptV2
referenceContractScript = PlutusScriptSerialised referenceContractScriptShortBs