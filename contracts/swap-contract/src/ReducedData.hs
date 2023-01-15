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
  ( signedBy
  , ownInput
  , getScriptOutputs
  , txInFromTxRef
  , SwapOutputDatum (..)
  , SwapTxOut (..)
  , SwapTxInInfo (..)
  , SwapScriptPurpose (..)
  , thisScriptInput
  , theseScriptOutputs
  , thoseScriptOutputs
  , swapTxInFromTxRef
  , isNInputs'
  , isNOutputs'
  , isAddrGettingPaidExactly'
  , isAddrHoldingExactlyToken'
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as PlutusV2
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

-- -- tx info
-- data SwapTxInfo = SwapTxInfo
--     { txInfoInputs          :: [SwapTxInInfo] -- Transaction inputs
--     , txInfoReferenceInputs :: BuiltinData
--     , txInfoOutputs         :: [SwapTxOut] -- Transaction outputs
--     , txInfoFee             :: BuiltinData
--     , txInfoMint            :: BuiltinData
--     , txInfoDCert           :: BuiltinData
--     , txInfoWdrl            :: BuiltinData
--     , txInfoValidRange      :: PlutusV2.POSIXTimeRange -- The valid range for the transaction.
--     , txInfoSignatories     :: [PlutusV2.PubKeyHash] -- Signatures provided with the transaction, attested that they all signed the tx
--     , txInfoRedeemers       :: BuiltinData
--     , txInfoData            :: BuiltinData
--     , txInfoId              :: BuiltinData
--     }
-- PlutusTx.unstableMakeIsData ''SwapTxInfo

-- | Purpose of the script that is currently running
data SwapScriptPurpose = Spending PlutusV2.TxOutRef
PlutusTx.makeIsDataIndexed ''SwapScriptPurpose [('Spending, 1)]

-- -- script context
-- data SwapScriptContext = SwapScriptContext
--   { scriptContextTxInfo  :: SwapTxInfo
--   , scriptContextPurpose :: SwapScriptPurpose 
--   }
-- PlutusTx.unstableMakeIsData ''SwapScriptContext
-------------------------------------------------------------------------------
-- | Rebuilt Functions
-------------------------------------------------------------------------------
-- | Check if a transaction was signed by the given public key.
{-# inlinable signedBy #-}
signedBy :: [PlutusV2.PubKeyHash] -> PlutusV2.PubKeyHash -> Bool
signedBy list k = loop list
  where
    loop [] = False
    loop (x:xs)
      | x == k = True
      | otherwise = loop xs

{-# inlinable theseScriptOutputs #-}
theseScriptOutputs :: [SwapTxOut] -> PlutusV2.Address -> [SwapTxOut]
theseScriptOutputs txOuts addr = [x | x <- txOuts, txOutAddress x == addr]

{-# INLINABLE thoseScriptOutputs #-}
thoseScriptOutputs :: PlutusV2.ValidatorHash -> [SwapTxOut] -> [(SwapOutputDatum, PlutusV2.Value)]
thoseScriptOutputs h p = [(d, txOutValue) | SwapTxOut{txOutDatum=d, txOutAddress=PlutusV2.Address (PlutusV2.ScriptCredential s) _, txOutValue} <- p, s == h]

-- get the validating script input
{-# inlinable thisScriptInput #-}
thisScriptInput :: [SwapTxInInfo] -> PlutusV2.TxOutRef -> SwapTxOut
thisScriptInput [] _ = traceError "script input not found"
thisScriptInput ((SwapTxInInfo tref ot) : tl) o_ref
  | tref == o_ref = ot
  | otherwise     = thisScriptInput tl o_ref

{-# inlinable swapTxInFromTxRef #-}
swapTxInFromTxRef :: [SwapTxInInfo] -> PlutusV2.TxOutRef -> SwapTxInInfo
swapTxInFromTxRef [] _ = traceError "Cant Find Tx In"
swapTxInFromTxRef (x:xs) outRef
  | txInInfoOutRef x == outRef = x
  | otherwise                  = swapTxInFromTxRef xs outRef

-- | Count the number of inputs that have datums of any kind.
{-# INLINABLE isNInputs' #-}
isNInputs' :: [SwapTxInInfo] -> Integer -> Bool
isNInputs' utxos number = loopInputs utxos 0
  where
    loopInputs :: [SwapTxInInfo] -> Integer -> Bool
    loopInputs []     !counter = counter == number
    loopInputs (x:xs) !counter = 
      case txOutDatum $ txInInfoResolved x of
        NoOutputDatum   -> loopInputs xs   counter
        (OutputDatum _) -> loopInputs xs ( counter + 1 ) -- inline

-- | Count the number of outputs going to the validator address
{-# INLINABLE isNOutputs' #-}
isNOutputs' :: [SwapTxOut] -> Integer -> Bool
isNOutputs' utxos number = loopInputs utxos 0
  where
    loopInputs :: [SwapTxOut] -> Integer  -> Bool
    loopInputs []     !counter = counter == number
    loopInputs (x:xs) !counter = 
      case txOutDatum x of
        NoOutputDatum   -> loopInputs xs   counter
        (OutputDatum _) -> loopInputs xs ( counter + 1 ) -- inline


{-# INLINABLE isAddrGettingPaidExactly' #-}
isAddrGettingPaidExactly' :: [SwapTxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
isAddrGettingPaidExactly' = loop 
  where
    loop :: [SwapTxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
    loop [] _ _ = False
    loop (x:xs) addr val = 
      let checkAddr = txOutAddress x == addr
          checkVal  = txOutValue x == val
          found     = checkAddr && checkVal
      in found || loop xs addr val

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

---- default functions to be removed
-- find the tx in
{-# inlinable txInFromTxRef #-}
txInFromTxRef :: [PlutusV2.TxInInfo] -> PlutusV2.TxOutRef -> PlutusV2.TxInInfo
txInFromTxRef [] _ = traceError "Cant Find Tx In"
txInFromTxRef (x:xs) outRef
  | PlutusV2.txInInfoOutRef x == outRef = x
  | otherwise                  = txInFromTxRef xs outRef


{-# inlinable getScriptOutputs #-}
getScriptOutputs :: [PlutusV2.TxOut] -> PlutusV2.Address -> [PlutusV2.TxOut]
getScriptOutputs txOuts addr' = getScriptOutputs' txOuts addr' []
  where
    getScriptOutputs' :: [PlutusV2.TxOut] -> PlutusV2.Address -> [PlutusV2.TxOut] -> [PlutusV2.TxOut]
    getScriptOutputs' [] _ contOuts = contOuts
    getScriptOutputs' (x:xs) addr contOuts
      | PlutusV2.txOutAddress x == addr = getScriptOutputs' xs addr (x:contOuts)
      | otherwise                       = getScriptOutputs' xs addr contOuts

-- rewrite findOwnInput without higher order functions
{-# inlinable ownInput #-}
ownInput :: PlutusV2.ScriptContext -> PlutusV2.TxOut
ownInput (PlutusV2.ScriptContext t_info (PlutusV2.Spending o_ref)) = getScriptInput (PlutusV2.txInfoInputs t_info) o_ref
ownInput _ = traceError "no script input"

-- get the validating script input
{-# inlinable getScriptInput #-}
getScriptInput :: [PlutusV2.TxInInfo] -> PlutusV2.TxOutRef -> PlutusV2.TxOut
getScriptInput [] _ = traceError "script input not found"
getScriptInput ((PlutusV2.TxInInfo tref ot) : tl) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput tl o_ref



