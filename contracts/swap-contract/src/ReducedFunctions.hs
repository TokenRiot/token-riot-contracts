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
module ReducedFunctions
  ( signedBy
  , checkMultisig
  , txInFromTxRef
  , findPayout
  , findTokenHolder
  , nInputs
  , nOutputs
  , nRedeemers
  , getScriptOutputs
  , getReferenceInput
  , ownInput
  , uniqueTokenName
  ) where
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api   as V2
import qualified Plutus.V1.Ledger.Value as Value
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
getReferenceInput :: [V2.TxInInfo] -> V2.ValidatorHash -> V2.TxOut
getReferenceInput txRefs vHash = getReferenceInput' txRefs
  where
    getReferenceInput' :: [V2.TxInInfo] -> V2.TxOut
    getReferenceInput' []     = traceError "no references"
    getReferenceInput' (x:xs)
      | checkHash x == True   = V2.txInInfoResolved x
      | otherwise             = getReferenceInput' xs
    
    checkHash :: V2.TxInInfo -> Bool
    checkHash (V2.TxInInfo _ (V2.TxOut (V2.Address (V2.ScriptCredential vHash') _) _ _ _)) = vHash == vHash' 
    checkHash _                                                                            = False

{-# inlinable getScriptOutputs #-}
getScriptOutputs :: [V2.TxOut] -> V2.Address -> [V2.TxOut]
getScriptOutputs txOuts addr' = getScriptOutputs' txOuts addr' []
  where
    getScriptOutputs' :: [V2.TxOut] -> V2.Address -> [V2.TxOut] -> [V2.TxOut]
    getScriptOutputs' []     _    contOuts = contOuts
    getScriptOutputs' (x:xs) addr contOuts
      | V2.txOutAddress x == addr = getScriptOutputs' xs addr (x:contOuts)
      | otherwise                 = getScriptOutputs' xs addr contOuts

-- rewrite findOwnInput without higher order functions
{-# inlinable ownInput #-}
ownInput :: V2.ScriptContext -> V2.TxOut
ownInput (V2.ScriptContext t_info (V2.Spending o_ref)) = getScriptInput (V2.txInfoInputs t_info) o_ref
ownInput _                                             = traceError "no script input"

-- get the validating script input
{-# inlinable getScriptInput #-}
getScriptInput :: [V2.TxInInfo] -> V2.TxOutRef -> V2.TxOut
getScriptInput []                           _     = traceError "script input not found"
getScriptInput ((V2.TxInInfo tref ot) : xs) o_ref
  | tref == o_ref                                 = ot
  | otherwise                                     = getScriptInput xs o_ref

{-# inlinable txInFromTxRef #-}
txInFromTxRef :: [V2.TxInInfo] -> V2.TxOutRef -> V2.TxInInfo
txInFromTxRef txIns outRef = txInFromTxRef' txIns
  where
    txInFromTxRef' :: [V2.TxInInfo] -> V2.TxInInfo
    txInFromTxRef' []                 = traceError "Cant Find Tx In"
    txInFromTxRef' (x:xs)
      | V2.txInInfoOutRef x == outRef = x
      | otherwise                     = txInFromTxRef' xs

-- | Check if a transaction was signed by the given public key.
{-# inlinable signedBy #-}
signedBy :: [V2.PubKeyHash] -> V2.PubKeyHash -> Bool
signedBy list k = loop list
  where
    loop []       = False
    loop (x:xs)
      | x == k    = True
      | otherwise = loop xs

{-# INLINABLE checkMultisig #-}
checkMultisig :: [V2.PubKeyHash] -> [V2.PubKeyHash] -> Integer -> Bool
checkMultisig signers pkhs thres = loopSigs pkhs 0
  where
    loopSigs :: [V2.PubKeyHash] -> Integer  -> Bool
    loopSigs []     !counter = counter >= thres
    loopSigs (x:xs) !counter = 
      if signedBy signers x
        then loopSigs xs (counter + 1) -- just add up the good sigs
        else loopSigs xs counter       -- toss out the bad

{-# INLINABLE findPayout #-}
findPayout :: [V2.TxOut] -> V2.Address -> V2.Value -> Bool
findPayout list addr val = isPayoutExact list
  where
    isPayoutExact :: [V2.TxOut] -> Bool
    isPayoutExact []          = False
    isPayoutExact (x:xs)
      | checkAddr && checkVal = True
      | otherwise             = isPayoutExact xs
      where
        checkAddr :: Bool
        checkAddr = V2.txOutAddress x == addr

        checkVal :: Bool
        checkVal = Value.geq (V2.txOutValue x) val

-- | Search a list of TxOut for a TxOut with a specific address that is hodling an exact amount of of a singular token.
findTokenHolder :: [V2.TxOut] -> V2.Address -> V2.CurrencySymbol -> V2.TokenName -> Integer -> Bool
findTokenHolder list addr pid tkn val = isAddrHoldingExactlyToken' list
  where
    isAddrHoldingExactlyToken' :: [V2.TxOut] -> Bool
    isAddrHoldingExactlyToken' []     = False
    isAddrHoldingExactlyToken' (x:xs)
      | checkAddr && checkVal         = True
      | otherwise                     = isAddrHoldingExactlyToken' xs
      where
        checkAddr :: Bool
        checkAddr = V2.txOutAddress x == addr

        checkVal :: Bool
        checkVal = Value.valueOf (V2.txOutValue x) pid tkn == val -- must be exact

-- | Count the number of inputs that have inline datums.
{-# INLINABLE nInputs #-}
nInputs :: [V2.TxInInfo] -> V2.Address -> Integer -> Bool
nInputs utxos addr number = loopInputs utxos 0 0
  where
    loopInputs :: [V2.TxInInfo] -> Integer -> Integer -> Bool
    loopInputs []     !dC !sC = (dC == number) && (sC == number)
    loopInputs (x:xs) !dC !sC = 
      case V2.txOutDatum txInOut of
        V2.NoOutputDatum       -> loopInputs xs dC sC
        (V2.OutputDatumHash _) -> loopInputs xs dC sC
        (V2.OutputDatum _)     -> 
          if V2.txOutAddress txInOut == addr
            then loopInputs xs (dC + 1) (sC + 1) -- inline
            else loopInputs xs (dC + 1) sC
      where 
        txInOut :: V2.TxOut
        txInOut = V2.txInInfoResolved x

-- | Count the number of outputs that have inline datums.
{-# INLINABLE nOutputs #-}
nOutputs :: [V2.TxOut] -> Integer -> Bool
nOutputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [V2.TxOut] -> Integer  -> Bool
    loopInputs []     !counter = counter == number
    loopInputs (x:xs) !counter = 
      case V2.txOutDatum x of
        V2.NoOutputDatum       -> loopInputs xs   counter
        (V2.OutputDatumHash _) -> loopInputs xs   counter
        (V2.OutputDatum _)     -> loopInputs xs ( counter + 1 ) -- inline

{-# INLINABLE nRedeemers #-}
nRedeemers :: [(V2.ScriptPurpose, V2.Redeemer)] -> Integer -> Bool
nRedeemers redeemers number = nRedeemers' redeemers 0
  where
    nRedeemers' :: [(V2.ScriptPurpose, V2.Redeemer)] -> Integer -> Bool
    nRedeemers' []          !counter = counter == number
    nRedeemers' ((_, _):xs) !counter = nRedeemers' xs ( counter + 1 )

{-# INLINABLE uniqueTokenName #-}
uniqueTokenName :: V2.BuiltinByteString -> V2.TxOutRef -> V2.TokenName
uniqueTokenName prefix txRef = V2.TokenName { V2.unTokenName = prependTxHash txHash index }
  where
    prependTxHash :: V2.BuiltinByteString -> Integer -> V2.BuiltinByteString
    prependTxHash string counter = sliceByteString 0 32 (appendByteString prefix (consByteString counter (sha3_256 string)))

    txHash :: V2.BuiltinByteString
    txHash = V2.getTxId $ V2.txOutRefId txRef

    index :: Integer
    index = V2.txOutRefIdx txRef