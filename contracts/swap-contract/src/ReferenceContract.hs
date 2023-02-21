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
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley                             ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.ByteString.Short                           as SBS
import qualified Plutus.V1.Ledger.Scripts                        as Scripts
import qualified Plutus.V2.Ledger.Api                            as V2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import           ReferenceDataType
import           ReducedFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Update
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [('Update, 0)]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ReferenceDatum -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case (datum, redeemer) of
    (Reference _ _ msd, Update) ->
      let !info      = V2.scriptContextTxInfo context
          !txSigners = V2.txInfoSignatories info
          !listOfPkh = mPkhs msd
          !threshold = mThres msd
      in traceIfFalse "multisig error" (checkMultisig txSigners listOfPkh threshold)
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: V2.Validator
validator' = V2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

referenceContractScriptShortBs :: SBS.ShortByteString
referenceContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

referenceContractScript :: PlutusScript PlutusScriptV2
referenceContractScript = PlutusScriptSerialised referenceContractScriptShortBs