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
module ReducedData
  ( SwapScriptContext (..)
  , SwapTxInfo (..)
  , SwapTxInInfo (..)
  , SwapTxOut (..)
  , findTxInByTxOutRef'
  , findOwnInput'
  , getContinuingOutputs'
  , txSignedBy'
  , isNInputs'
  , isNOutputs'
  , isAddrGettingPaidExactly'
  , isAddrHoldingExactlyToken'
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api   as PlutusV2
import           Plutus.V1.Ledger.Value as Value
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Min Max Data Structures
-------------------------------------------------------------------------------

-- tx out (outputs)
data SwapTxOut = SwapTxOut
  { txOutAddress         :: PlutusV2.Address
  , txOutValue           :: PlutusV2.Value
  , txOutDatum           :: PlutusV2.OutputDatum
  , txOutReferenceScript :: BuiltinData
  }
PlutusTx.unstableMakeIsData ''SwapTxOut

-- tx in info (inputs)
data SwapTxInInfo = SwapTxInInfo
    { txInInfoOutRef   :: PlutusV2.TxOutRef
    , txInInfoResolved :: SwapTxOut
    } 
PlutusTx.unstableMakeIsData ''SwapTxInInfo

-- tx info
data SwapTxInfo = SwapTxInfo
    { txInfoInputs          :: [SwapTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: BuiltinData
    , txInfoOutputs         :: [SwapTxOut] -- Transaction outputs
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: BuiltinData
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: PlutusV2.POSIXTimeRange -- The valid range for the transaction.
    , txInfoSignatories     :: [PlutusV2.PubKeyHash] -- Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''SwapTxInfo

-- script context
data SwapScriptContext = SwapScriptContext
  { scriptContextTxInfo  :: SwapTxInfo
  , scriptContextPurpose :: PlutusV2.ScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''SwapScriptContext
-------------------------------------------------------------------------------
-- | Rebuilt Functions
-------------------------------------------------------------------------------

-- find the tx in 
findTxInByTxOutRef' :: PlutusV2.TxOutRef -> SwapTxInfo -> Maybe SwapTxInInfo
findTxInByTxOutRef' outRef SwapTxInfo{txInfoInputs} =
    find (\SwapTxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs

-- | Find the input currently being validated.
findOwnInput' :: SwapScriptContext -> Maybe SwapTxInInfo
findOwnInput' SwapScriptContext{scriptContextTxInfo=SwapTxInfo{txInfoInputs}, scriptContextPurpose=PlutusV2.Spending txOutRef} =
    find (\SwapTxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
findOwnInput' _ = Nothing

-- | Get all the outputs that pay to the same script address we are currently spending from, if any.
getContinuingOutputs' :: SwapScriptContext -> [SwapTxOut]
getContinuingOutputs' ctx | Just SwapTxInInfo{txInInfoResolved=SwapTxOut{txOutAddress}} <- findOwnInput' ctx = filter (f txOutAddress) (txInfoOutputs $ scriptContextTxInfo ctx)
    where
        f addr SwapTxOut{txOutAddress=otherAddress} = addr == otherAddress
getContinuingOutputs' _ = traceError "Lf" -- "Can't get any continuing outputs"

-- | Check if a transaction was signed by the given public key.
txSignedBy' :: SwapTxInfo -> PlutusV2.PubKeyHash -> Bool
txSignedBy' SwapTxInfo{txInfoSignatories} k = case find ((==) k) txInfoSignatories of
    Just _  -> True
    Nothing -> False

-- | Count the number of inputs that have datums of any kind.
isNInputs' :: [SwapTxInInfo] -> Integer -> Bool
isNInputs' utxos number = loopInputs utxos 0
  where
    loopInputs :: [SwapTxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case txOutDatum $ txInInfoResolved x of
        PlutusV2.NoOutputDatum         -> loopInputs xs   counter
        ( PlutusV2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( PlutusV2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline

-- | Count the number of outputs that have datums of any kind.
isNOutputs' :: [SwapTxOut] -> Integer -> Bool
isNOutputs' utxos number = loopInputs utxos 0
  where
    loopInputs :: [SwapTxOut] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case txOutDatum x of
        PlutusV2.NoOutputDatum         -> loopInputs xs   counter
        ( PlutusV2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( PlutusV2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline

-- | Search a list of TxOut for a TxOut with a specific address that is hodling an exact amount of of a singular token. 
isAddrGettingPaidExactly' :: [SwapTxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
isAddrGettingPaidExactly' []     _    _   = False
isAddrGettingPaidExactly' (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = isAddrGettingPaidExactly' xs addr val
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == addr

    checkVal :: Bool
    checkVal = txOutValue x == val     -- must be exact

-- | Search a list of TxOut for a TxOut with a specific address that is hodling an exact amount of of a singular token.
isAddrHoldingExactlyToken' :: [SwapTxOut] -> PlutusV2.Address -> PlutusV2.CurrencySymbol -> PlutusV2.TokenName -> Integer -> Bool
isAddrHoldingExactlyToken' []     _    _   _   _   = False
isAddrHoldingExactlyToken' (x:xs) addr pid tkn val
  | checkAddr && checkVal = True
  | otherwise             = isAddrHoldingExactlyToken' xs addr pid tkn val 
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == addr

    checkVal :: Bool
    checkVal = Value.valueOf (txOutValue x) pid tkn == val -- must be exact