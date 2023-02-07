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
  , referenceContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Api           as V2
import qualified Plutus.V2.Ledger.Contexts      as V2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import           ReferenceDataType
import           UsefulFuncs                    (createBuiltinByteString)
import           ReducedFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-- Hardcoded Multisig
multiPkh1 :: V2.PubKeyHash
multiPkh1 = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString [111, 255, 109, 51, 149, 143, 101, 2, 45, 230, 74, 171, 211, 193, 106, 122, 126, 7, 186, 215, 169, 74, 69, 133, 206, 29, 172, 118] }

multiPkh2 :: V2.PubKeyHash
multiPkh2 = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString [63, 22, 163, 63, 160, 112, 117, 22, 247, 235, 164, 210, 223, 197, 124, 214, 18, 122, 160, 94, 171, 114, 217, 90, 27, 249, 82, 99] }

multiPkh3 :: V2.PubKeyHash
multiPkh3 = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString [60, 47, 234, 52, 123, 28, 154, 240, 143, 240, 62, 109, 37, 231, 123, 240, 32, 118, 204, 101, 205, 133, 30, 131, 27, 182, 139, 132] }

-- all possible signers
listOfPkh :: [V2.PubKeyHash]
listOfPkh = [multiPkh1, multiPkh2, multiPkh3]
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
    (Reference _ _, Update) ->
      let !info      = V2.scriptContextTxInfo context
          !txSigners = V2.txInfoSignatories info
      in traceIfFalse "mul" (checkMultisig txSigners listOfPkh 2)
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