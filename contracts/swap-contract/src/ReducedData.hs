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
{-# LANGUAGE ViewPatterns          #-}
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
  , SwapOutputDatum (..)
  , findTxInByTxOutRef'
  , ownInput
  , signedBy
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
-- output datums
data SwapOutputDatum =  NoOutputDatum | OutputDatum PlutusV2.Datum
PlutusTx.makeIsDataIndexed ''SwapOutputDatum [('NoOutputDatum, 0), ('OutputDatum, 2)]

-- tx out (outputs)
data SwapTxOut = SwapTxOut
  { txOutAddress         :: PlutusV2.Address
  , txOutValue           :: PlutusV2.Value
  , txOutDatum           :: SwapOutputDatum
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

-- | Purpose of the script that is currently running
data SwapScriptPurpose = Spending PlutusV2.TxOutRef
PlutusTx.makeIsDataIndexed ''SwapScriptPurpose [('Spending, 1)]

-- script context
data SwapScriptContext = SwapScriptContext
  { scriptContextTxInfo  :: SwapTxInfo
  , scriptContextPurpose :: SwapScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''SwapScriptContext
-------------------------------------------------------------------------------
-- | Rebuilt Functions
-------------------------------------------------------------------------------
-- find the tx in 
findTxInByTxOutRef' :: PlutusV2.TxOutRef -> SwapTxInfo -> Maybe SwapTxInInfo
findTxInByTxOutRef' outRef SwapTxInfo{txInfoInputs} =
    find (\SwapTxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs

-- rewrite findOwnInput without higher order functions
{-# inlinable ownInput #-}
ownInput :: SwapScriptContext -> SwapTxOut
ownInput (SwapScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref

-- get the validating script input
{-# inlinable getScriptInput #-}
getScriptInput :: [SwapTxInInfo] -> PlutusV2.TxOutRef -> SwapTxOut
getScriptInput [] _ = traceError "script input not found"
getScriptInput ((SwapTxInInfo tref ot) : tl) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput tl o_ref

-- | Check if a transaction was signed by the given public key.
{-# inlinable signedBy #-}
signedBy :: [PlutusV2.PubKeyHash] -> PlutusV2.PubKeyHash -> Bool
signedBy []     _ = False
signedBy (x:xs) k
  | x == k    = True
  | otherwise =  signedBy xs k

-- | Count the number of inputs that have datums of any kind.
isNInputs' :: [SwapTxInInfo] -> PlutusV2.Address -> Integer -> Bool
isNInputs' utxos addr number = loopInputs utxos 0
  where
    loopInputs :: [SwapTxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) !counter =
      if (txOutAddress $ txInInfoResolved x) == addr
        then loopInputs xs (counter + 1)
        else loopInputs xs counter

-- | Count the number of outputs going to the validator address
isNOutputs' :: [SwapTxOut] -> PlutusV2.Address -> Integer -> Bool
isNOutputs' utxos addr number = loopInputs utxos 0
  where
    loopInputs :: [SwapTxOut] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) !counter =
      if txOutAddress x == addr
        then loopInputs xs (counter + 1)
        else loopInputs xs counter

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