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
module SwapContract
  ( swapContractScript
  , ScriptParameters(..)
  ) where
import           Cardano.Api.Shelley    ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import           OptimizerOptions       ( theOptimizerOptions )
import           Plutonomy
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as V2
import           ReducedFunctions
import           ReferenceDataType
import           SwappableDataType
import qualified UsefulFuncs            as UF
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

{-# INLINABLE calculateServiceFee #-}
calculateServiceFee :: CustomDatumType -> ReferenceDatum -> Integer
calculateServiceFee (Swappable _ pd _) (Reference _ sf _ _) =
  if (pPid pd == Value.adaSymbol) && (pTkn pd == Value.adaToken)
  then if percentFee > sFee then percentFee else sFee
  else sFee
  where
    percentFee :: Integer
    percentFee = divide (pAmt pd) (servicePerc sf)

    sFee :: Integer
    sFee = serviceFee sf
calculateServiceFee _ (Reference _ sf _ _) = serviceFee sf

{-# INLINABLE checkServiceFeePayout#-}
checkServiceFeePayout :: CustomDatumType -> ReferenceDatum -> [V2.TxOut] -> Bool
checkServiceFeePayout d r txOutputs = (findPayout txOutputs (cashAddr r) feeValue)
  where
    cashAddr :: ReferenceDatum -> V2.Address
    cashAddr (Reference ca _ _ _) = UF.createAddress (caPkh ca) (caSc ca)
  
    feeValue :: V2.Value
    feeValue = UF.adaValue (calculateServiceFee d r)

{-# INLINABLE checkCancellationFeePayout #-}
checkCancellationFeePayout :: ReferenceDatum -> [V2.TxOut] -> Bool
checkCancellationFeePayout (Reference ca sf _ _) txOutputs = (findPayout txOutputs cashAddr feeValue)
  where
    cashAddr :: V2.Address
    cashAddr = UF.createAddress (caPkh ca) (caSc ca)
  
    feeValue :: V2.Value
    feeValue = Value.singleton Value.adaSymbol Value.adaToken (cancellationFee sf)

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType
  = Swappable PayToData PaymentData TimeData
  | Auctioning PayToData TimeData TimeData
  | Offering PayToData MakeOfferData OfferFlagData PaymentData
  | Bidding PayToData MakeOfferData PaymentData
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ( 'Swappable,  0 )
                                              , ( 'Auctioning, 1 )
                                              , ( 'Offering,   2 )
                                              , ( 'Bidding,    3 )
                                              ]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType 
  = Remove
  | FlatRate PayToData ADAIncData SpecificToken
  | AcceptOffer ADAIncData MakeOfferData
  | Update ADAIncData
  | Complete
  | Transform
  | FlatRateRemove PayToData SpecificToken
  | AcceptOfferRemove MakeOfferData
  | CancelTimeLock
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove,            0 )
                                                , ( 'FlatRate,          1 )
                                                , ( 'AcceptOffer,       2 )
                                                , ( 'Update,            3 )
                                                , ( 'Complete,          4 )
                                                , ( 'Transform,         5 )
                                                , ( 'FlatRateRemove,    6 )
                                                , ( 'AcceptOfferRemove, 7 )
                                                , ( 'CancelTimeLock,    8 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: ScriptParameters -> CustomDatumType -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator ScriptParameters {..} datum redeemer context =
  case (datum, redeemer) of
--Swapping---------------------------------------------------------------------
    {- | Swappable State

      Different ways to swap UTxO walletship.

      Redeemers will determine the type of swap being used. Unless the UTxO is being
      explicity removed, the UTxO is assumed to be continuing inside the contract.

      User's are allowed to update and remove their utxos at will if not time locked.

      To Lock some UTxO for some pre-defined interval of time. The datum expects
      input integers that are greater than or equal to zero.

      echo `expr $(echo $(date +%s%3N)) + $(echo 0)`
      # 1659817471786

      A five (5) minute window would be 5 * 60 * 1000  = 300,000.
      
      echo `expr $(echo $(date +%s%3N)) + $(echo 300000)`
      # 1659817771786
    -}
    
    -- | A trader may transform their UTxO, holding the owner constant, changing the value and time.
    (Swappable ptd _ td, Transform) ->
      let !walletPkh        = ptPkh ptd
          !info             = V2.scriptContextTxInfo context
          !txInputs         = V2.txInfoInputs info
          !txOutputs        = V2.txInfoOutputs info
          !lockTimeInterval = UF.lockBetweenTimeInterval (tStart td) (tEnd td)
          !txValidityRange  = V2.txInfoValidRange info
          !txSigners        = V2.txInfoSignatories info
          !validatingInput  = ownInput context
          !scriptAddr       = V2.txOutAddress validatingInput
          !contTxOutputs    = getScriptOutputs txOutputs scriptAddr
      in case getOutboundDatum contTxOutputs of
        -- transform a swappable utxo
        (Swappable ptd' _ td') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in
          && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                                -- single going out
          && traceIfFalse "owns" (ptd == ptd')                                             -- seller cant change
          && traceIfFalse "time" (checkValidTimeLock td td')                               -- valid time lock
          && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it

        -- transform utxo into an offer
        (Offering ptd' _ _ _) -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in
          && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                                -- single going out
          && traceIfFalse "owns" (ptd == ptd')                                             -- seller cant change
          && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
        
        -- transform a swappable state into the auctioning state
        (Auctioning ptd' atd td') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
          && traceIfFalse "owns" (ptd == ptd')                                             -- seller cant change
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in
          && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                                -- single going out
          && traceIfFalse "Auct" (checkValidTimeData atd)                                  -- valid auction time lock
          && traceIfFalse "time" (checkValidTimeLock td td')                               -- valid time lock
          && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
        
        -- transform a swappable state into a bid
        (Bidding ptd' _ _) -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in
          && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                                -- single going out
          && traceIfFalse "owns" (ptd == ptd')                                             -- seller cant change
          && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
    
    -- | A trader may update their UTxO, holding validating value constant, incrementing the min ada, and changing the payment datum.
    (Swappable ptd _ td, Update aid) ->
      let !walletPkh       = ptPkh ptd
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !thisValue       = V2.txOutValue validatingInput
          !scriptAddr      = V2.txOutAddress validatingInput
          !contTxOutputs   = getScriptOutputs txOutputs scriptAddr
          !incomingValue   = thisValue + UF.adaValue (adaInc aid)
      in case getOutboundDatumByValue contTxOutputs incomingValue of
        -- update the payment data on a swappable state
        (Swappable ptd' _ td') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
          && traceIfFalse "Datu" (ptd == ptd')                   -- seller and time can't change
          && traceIfFalse "Time" (td == td')                     -- seller and time can't change
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, single going out
          && traceIfFalse "Outs" (nOutputs contTxOutputs 1)      -- single tx going in, single going out

        -- Update a swappable state into the auctioning state
        (Auctioning ptd' atd td') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
          && traceIfFalse "Datu" (ptd == ptd')                   -- seller can't change
          && traceIfFalse "Time" (td == td')                     -- time can't change
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1) -- single tx going in
          && traceIfFalse "Outs" (nOutputs contTxOutputs 1)      -- single going out
          && traceIfFalse "Chan" (checkValidTimeData atd)        -- valid time lock

        -- Other Datums fail
        _ -> traceIfFalse "Swappable:Update:Undefined Datum" False
    
    -- | A trader may remove their UTxO if not currently being timelocked.
    (Swappable ptd _ td, Remove) ->
      let !walletPkh        = ptPkh ptd
          !walletAddr       = UF.createAddress walletPkh (ptSc ptd)
          !info             = V2.scriptContextTxInfo context
          !txInputs         = V2.txInfoInputs info
          !txOutputs        = V2.txInfoOutputs info
          !txSigners        = V2.txInfoSignatories info
          !validatingInput  = ownInput context
          !thisValue        = V2.txOutValue validatingInput
          !scriptAddr       = V2.txOutAddress validatingInput
          !lockTimeInterval = UF.lockBetweenTimeInterval (tStart td) (tEnd td)
          !txValidityRange  = V2.txInfoValidRange info
      in traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
      && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue)               -- seller must get the UTxO
      && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in, no continue
      && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it

    -- | A trader may cancel their timelock by paying a fee.
    (Swappable ptd _ td, CancelTimeLock) ->
      let !walletPkh        = ptPkh ptd
          !walletAddr       = UF.createAddress walletPkh (ptSc ptd)
          !info             = V2.scriptContextTxInfo context
          !txInputs         = V2.txInfoInputs info
          !txOutputs        = V2.txInfoOutputs info
          !txSigners        = V2.txInfoSignatories info
          !validatingInput  = ownInput context
          !thisValue        = V2.txOutValue validatingInput
          !scriptAddr       = V2.txOutAddress validatingInput
          !lockTimeInterval = UF.lockBetweenTimeInterval (tStart td) (tEnd td)
          !txValidityRange  = V2.txInfoValidRange info
          !refTxIns         = V2.txInfoReferenceInputs info
          !refTxOut         = getReferenceInput refTxIns refHash
          !refDatum         = getReferenceDatum refTxOut
          !refValue         = V2.txOutValue refTxOut
      in traceIfFalse "Sign" (signedBy txSigners walletPkh)                                  -- seller must sign it
      && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue)                     -- seller must get the UTxO
      && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                                 -- single tx going in, no continue
      && traceIfFalse "Lock" (not $ UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
      && traceIfFalse "fee"  (checkCancellationFeePayout refDatum txOutputs)                 -- check if paying fee
      && traceIfFalse "val"  (Value.valueOf refValue lockPid lockTkn == 1)                   -- check if correct reference
    
    -- | Flat rate swap of UTxO for an predefined amount of a single token.
    (Swappable ptd pd td, FlatRate ptd' aid st) ->
      let !walletAddr      = UF.createAddress (ptPkh ptd) (ptSc ptd)
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !thisValue       = V2.txOutValue validatingInput
          !scriptAddr      = V2.txOutAddress validatingInput
          !contTxOutputs   = getScriptOutputs txOutputs scriptAddr
          !refTxIns        = V2.txInfoReferenceInputs info
          !refTxOut        = getReferenceInput refTxIns refHash
          !refDatum        = getReferenceDatum refTxOut
          !refValue        = V2.txOutValue refTxOut
          !incomingValue   = thisValue + UF.adaValue (adaInc aid)
          !thisTkn         = getTokenName pd st
      in case getOutboundDatumByValue contTxOutputs incomingValue of
        -- swappable only
        (Swappable ptd'' _ td') -> 
             traceIfFalse "Pays" (findTokenHolder txOutputs walletAddr (pPid pd) thisTkn (pAmt pd)) -- seller must be paid
          && traceIfFalse "oldo" (ptd /= ptd'')                                                     -- cant sell this
          && traceIfFalse "newo" (ptd' == ptd'')                                                    -- new owner must own it
          && traceIfFalse "time" (td == td')                                                        -- time data must remain
          && traceIfFalse "Empt" (pAmt pd /= 0)                                                     -- seller must define price
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                                    -- single tx going in
          && traceIfFalse "outs" (nOutputs contTxOutputs 1)                                         -- single going out
          && traceIfFalse "sign" (signedBy txSigners (ptPkh ptd'))                                  -- buyer must sign
          && traceIfFalse "fee"  (checkServiceFeePayout (Swappable ptd pd td) refDatum txOutputs)   -- check if paying fee
          && traceIfFalse "val"  (Value.valueOf refValue lockPid lockTkn == 1)                      -- check if correct reference

        -- other datums fail
        _ -> traceIfFalse "Swappable:FlatRate:Undefined Datum" False
    
    -- | Flat rate purchase into buyer wallet of UTxO for an predefined amount of a single token.
    (Swappable ptd pd td, FlatRateRemove ptd' st) ->
      let !walletAddr       = UF.createAddress (ptPkh ptd) (ptSc ptd)
          !buyerPkh         = ptPkh ptd'
          !buyerAddr        = UF.createAddress buyerPkh (ptSc ptd')
          !info             = V2.scriptContextTxInfo context
          !txInputs         = V2.txInfoInputs info
          !txOutputs        = V2.txInfoOutputs info
          !txSigners        = V2.txInfoSignatories info
          !validatingInput  = ownInput context
          !thisValue        = V2.txOutValue validatingInput
          !scriptAddr       = V2.txOutAddress validatingInput
          !lockTimeInterval = UF.lockBetweenTimeInterval (tStart td) (tEnd td)
          !txValidityRange  = V2.txInfoValidRange info
          !refTxIns         = V2.txInfoReferenceInputs info
          !refTxOut         = getReferenceInput refTxIns refHash
          !refDatum         = getReferenceDatum refTxOut
          !refValue         = V2.txOutValue refTxOut
          !thisTkn          = getTokenName pd st
      in traceIfFalse "Signer" (signedBy txSigners buyerPkh)                                      -- seller must sign it
      && traceIfFalse "Tokens" (findTokenHolder txOutputs walletAddr (pPid pd) thisTkn (pAmt pd)) -- seller must be paid
      && traceIfFalse "Pays"   (findPayout txOutputs buyerAddr thisValue)                         -- buyer must be paid
      && traceIfFalse "Empty"  (pAmt pd /= 0)                                                     -- seller must define price
      && traceIfFalse "Ins"    (nInputs txInputs scriptAddr 1)                                    -- single tx going in
      && traceIfFalse "Lock"   (UF.isTxOutsideInterval lockTimeInterval txValidityRange)          -- seller can unlock it
      && traceIfFalse "fee"    (checkServiceFeePayout (Swappable ptd pd td) refDatum txOutputs)   -- check if paying fee
      && traceIfFalse "val"    (Value.valueOf refValue lockPid lockTkn == 1)                      -- check if correct reference
    
    -- | AcceptOffer to change walletship of UTxO for some amount of a single token + extras.
    (Swappable ptd _ td, AcceptOffer aid mod) ->
      let !txId            = createTxOutRef (moTx mod) (moIdx mod)
          !walletPkh       = ptPkh ptd
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !thisValue       = V2.txOutValue validatingInput
          !scriptAddr      = V2.txOutAddress validatingInput
          !contTxOutputs   = getScriptOutputs txOutputs scriptAddr
          !incomingValue   = thisValue + UF.adaValue (adaInc aid)
      in case getDatumByTxId txId txInputs of
        -- offering only
        (Offering ptd' _ ofd pd') ->
          case getOutboundDatumByValue contTxOutputs incomingValue of
            -- cont into swappable only
            (Swappable ptd'' pd'' td') -> 
                 traceIfFalse "sign" (signedBy txSigners walletPkh)  -- seller must sign
              && traceIfFalse "oldo" (ptd /= ptd'')                  -- cant sell this to self
              && traceIfFalse "newo" (ptd' == ptd'')                 -- new owner must own it
              && traceIfFalse "time" (td == td')                     -- time data must remain
              && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 2) -- single tx going in
              && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
              && traceIfFalse "PayD" (pd' == pd'')                   -- payment data must be from offer
              && traceIfFalse "Flag" (oFlag ofd == 0)                -- Offer stays in contract

            -- other datums fail
            _ -> traceIfFalse "Swappable:Offer:Not In Offer State" False
        
        -- anything else fails
        _ -> traceIfFalse "Swappable:Offer:Undefined Datum" False

    -- | Offer but remove it to a the buyer's wallet
    (Swappable ptd _ td, AcceptOfferRemove mod) ->
      let !txId             = createTxOutRef (moTx mod) (moIdx mod)
          !walletPkh        = ptPkh ptd
          !info             = V2.scriptContextTxInfo context
          !txInputs         = V2.txInfoInputs info
          !txOutputs        = V2.txInfoOutputs info
          !txSigners        = V2.txInfoSignatories info
          !validatingInput  = ownInput context
          !thisValue        = V2.txOutValue validatingInput
          !scriptAddr       = V2.txOutAddress validatingInput
          !lockTimeInterval = UF.lockBetweenTimeInterval (tStart td) (tEnd td)
          !txValidityRange  = V2.txInfoValidRange info
      in case getDatumByTxId txId txInputs of
        -- offering only
        (Offering ptd' _ ofd _) -> 
          let !buyerAddr = UF.createAddress (ptPkh ptd') (ptSc ptd')
          in traceIfFalse "sign" (signedBy txSigners walletPkh)                            -- seller must sign
          && traceIfFalse "oldo" (ptd /= ptd')                                             -- cant sell this to self
          && traceIfFalse "Pays" (findPayout txOutputs buyerAddr thisValue)                -- buyer must be paid
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 2)                           -- double tx going in
          && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
          && traceIfFalse "Flag" (oFlag ofd /= 0)                                          -- Offer stays in contract
        
        -- anything else fails
        _ -> traceIfFalse "Swappable:Offering:Undefined Datum" False
--Offering---------------------------------------------------------------------
    {- | Offering State

      Allows many users to store their offers inside the contract for some offer 
      trade to occur. An offerer may remove their current offers or transform their
      offer into a new offer. To ensure that a specific offer goes to a specific 
      UTxO, the TxID needs to attached of the UTxO an offerer is making an offer.

      The TxId in the Offering datum is the TxId of the UTxO that an offer is 
      being made on. If the TxId changes then the offerer will need to transform
      their offer to account for TxId change.

    -}

    -- | Remove the UTxO from the contract.
    (Offering ptd _ _ _, Remove) ->
      let !walletPkh        = ptPkh ptd
          !walletAddr       = UF.createAddress walletPkh (ptSc ptd)
          !info             = V2.scriptContextTxInfo context
          !txInputs         = V2.txInfoInputs info
          !txOutputs        = V2.txInfoOutputs info
          !txSigners        = V2.txInfoSignatories info
          !validatingInput  = ownInput context
          !thisValue        = V2.txOutValue validatingInput
          !scriptAddr       = V2.txOutAddress validatingInput
      in traceIfFalse "Sign" (signedBy txSigners walletPkh)              -- seller must sign it
      && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue) -- seller must get the UTxO
      && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)             -- single tx going in, no continue

    -- | Transform the make offer tx ref info
    (Offering ptd _ _ _, Transform) ->
      let !walletPkh       = ptPkh ptd
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !scriptAddr      = V2.txOutAddress validatingInput
          !contTxOutputs   = getScriptOutputs txOutputs scriptAddr
      in case getOutboundDatum contTxOutputs of
        -- offering only
        (Offering ptd' _ _ _) -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, no continue
          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
          && traceIfFalse "owns" (ptd == ptd')                   -- seller cant change

        -- offer into a swappable
        (Swappable ptd' _ td') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, no continue
          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
          && traceIfFalse "owns" (ptd == ptd')                   -- seller cant change
          && traceIfFalse "Auct" (checkValidTimeData td')        -- valid auction time lock
        
        -- other endpoints fail
        _ -> traceIfFalse "Offering:Transform:Undefined Datum" False

    -- | Complete an offer with a specific swappable UTxO.
    (Offering ptd mod ofd pd, Complete) ->
      let !txId            = createTxOutRef (moTx mod) (moIdx mod)
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !thisValue       = V2.txOutValue validatingInput
          !scriptAddr      = V2.txOutAddress validatingInput
          !refTxIns        = V2.txInfoReferenceInputs info
          !refTxOut        = getReferenceInput refTxIns refHash
          !refDatum        = getReferenceDatum refTxOut
          !refValue        = V2.txOutValue refTxOut
      in case getDatumByTxId txId txInputs of
        -- swappable only
        (Swappable ptd' _ _) ->
          let !sellerPkh  = ptPkh ptd'
              !sellerAddr = UF.createAddress sellerPkh (ptSc ptd')
          in traceIfFalse "Sign" (signedBy txSigners sellerPkh)                                       -- seller must sign it
          && traceIfFalse "oldo" (ptd /= ptd')                                                        -- cant sell this to self
          && traceIfFalse "pays" (findPayout txOutputs sellerAddr thisValue)                          -- seller must get the UTxO
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 2)                                      -- double tx going in
          && traceIfFalse "fee"  (checkServiceFeePayout (Offering ptd mod ofd pd) refDatum txOutputs) -- check if paying fee
          && traceIfFalse "val"  (Value.valueOf refValue lockPid lockTkn == 1)                        -- check if correct reference
        
        -- anything else fails
        _ -> traceIfFalse "Offering:Complete:Undefined Datum" False
--Auctioning-------------------------------------------------------------------
    {- | Auctioning State
      
      Allows a UTxO to be auctioned for some amount of time. Successful auctions are
      placed back into the swappable state as the auction state is a time lock. Similarly 
      to the swappable state, auctions remain in the contract after completion.

      The auction start time and end time are set in the same fashion as the time locking state.

      echo `expr $(echo $(date +%s%3N)) + $(echo 0)`
      # 1659817471786

      A five (5) minute window would be 5 * 60 * 1000  = 300,000.
      
      echo `expr $(echo $(date +%s%3N)) + $(echo 300000)`
      # 1659817771786

    -}

    -- | Remove the UTxO from the contract before the auction starts or after a failed auction.
    (Auctioning ptd atd gtd, Remove) ->
      let !walletPkh           = ptPkh ptd
          !walletAddr          = UF.createAddress walletPkh (ptSc ptd)
          !lockTimeInterval    = UF.lockBetweenTimeInterval (tStart gtd) (tEnd gtd)
          !auctionTimeInterval = UF.lockBetweenTimeInterval (tStart atd) (tEnd atd)
          !info                = V2.scriptContextTxInfo context
          !txValidityRange     = V2.txInfoValidRange info
          !txSigners           = V2.txInfoSignatories info
          !txInputs            = V2.txInfoInputs info
          !txOutputs           = V2.txInfoOutputs info
          !validatingInput     = ownInput context
          !thisValue           = V2.txOutValue validatingInput
          !scriptAddr          = V2.txOutAddress validatingInput
      in traceIfFalse "Sign" (signedBy txSigners walletPkh)                               -- seller must sign it
      && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue)                  -- seller must get the UTxO
      && traceIfFalse "Lock" (UF.isTxOutsideInterval lockTimeInterval txValidityRange)    -- seller can unlock it
      && traceIfFalse "Auct" (UF.isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it
      && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                              -- single tx going in, no continue

    -- | Update the auction back into the swappable state.
    (Auctioning ptd atd gtd, Update aid) ->
      let !walletPkh           = ptPkh ptd
          !info                = V2.scriptContextTxInfo context
          !auctionTimeInterval = UF.lockBetweenTimeInterval (tStart atd) (tEnd atd)
          !txValidityRange     = V2.txInfoValidRange info
          !txSigners           = V2.txInfoSignatories info
          !txInputs            = V2.txInfoInputs info
          !txOutputs           = V2.txInfoOutputs info
          !validatingInput     = ownInput context
          !thisValue           = V2.txOutValue validatingInput
          !scriptAddr          = V2.txOutAddress validatingInput
          !contTxOutputs       = getScriptOutputs txOutputs scriptAddr
          !incomingValue       = thisValue + UF.adaValue (adaInc aid)
      in case getOutboundDatumByValue contTxOutputs incomingValue of
        -- go back to the swap state
        (Swappable ptd' _ gtd') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)                               -- seller must sign it
          && traceIfFalse "Auct" (UF.isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it
          && traceIfFalse "newo" (ptd == ptd')                                                -- new owner must own it
          && traceIfFalse "time" (gtd == gtd')                                                -- time data must remain
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                              -- single tx going in, no continue
          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)                                   -- single going out
  
        -- anything else fails
        _ -> traceIfFalse "Auctioning:Update:Undefined Datum" False
    
    -- | Offer the auction for some selected bid
    (Auctioning ptd atd gtd, AcceptOffer aid mod) ->
      let !txId                = createTxOutRef (moTx mod) (moIdx mod)
          !info                = V2.scriptContextTxInfo context
          !walletPkh           = ptPkh ptd
          !auctionTimeInterval = UF.lockBetweenTimeInterval (tStart atd) (tEnd atd)
          !txValidityRange     = V2.txInfoValidRange info
          !txSigners           = V2.txInfoSignatories info
          !txInputs            = V2.txInfoInputs info
          !txOutputs           = V2.txInfoOutputs info
          !validatingInput     = ownInput context
          !thisValue           = V2.txOutValue validatingInput
          !scriptAddr          = V2.txOutAddress validatingInput
          !contTxOutputs       = getScriptOutputs txOutputs scriptAddr
          !incomingValue       = thisValue + UF.adaValue (adaInc aid)
      in case getDatumByTxId txId txInputs of
        -- offering only
        (Bidding ptd' _ pd') ->
          case getOutboundDatumByValue contTxOutputs incomingValue of
            -- cont into swappable only
            (Swappable ptd'' pd'' td') -> 
                 traceIfFalse "sign" (signedBy txSigners walletPkh)                               -- seller must sign
              && traceIfFalse "oldo" (ptd /= ptd'')                                               -- cant sell this to self
              && traceIfFalse "newo" (ptd' == ptd'')                                              -- new owner must own it
              && traceIfFalse "time" (gtd == td')                                                 -- time data must remain
              && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 2)                              -- double tx going in
              && traceIfFalse "Out"  (nOutputs contTxOutputs 1)                                   -- single going out
              && traceIfFalse "PayD" (pd' == pd'')                                                -- payment data must be from bid
              && traceIfFalse "Auct" (UF.isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it

            -- other datums fail
            _ -> traceIfFalse "Auctioning:AcceptOffer:Undefined Datum" False
        
        -- anything else fails
        _ -> traceIfFalse "Auctioning:Offering:Undefined Datum" False
--Bidding----------------------------------------------------------------------
    {- | Bidding State 

      Allows a bidder to place their bid into the contract for an on going auction. At the end of
      the auction, the auctioner will select the best bid and complete the auction or they may reject
      all bids and remove the UTxO back into the swap state. Similar to the offer state, bidders will
      need to remove old bids when the auction is over. They may also choose to transform their bid for
      a new auction or to increase the bid by changing the value.
    
    -}
    
    -- | Remove the UTxO from the contract.
    (Bidding ptd _ _, Remove) ->
      let !walletPkh           = ptPkh ptd
          !walletAddr          = UF.createAddress walletPkh (ptSc ptd)
          !info                = V2.scriptContextTxInfo context
          !txSigners           = V2.txInfoSignatories info
          !txInputs            = V2.txInfoInputs info
          !txOutputs           = V2.txInfoOutputs info
          !validatingInput     = ownInput context
          !thisValue           = V2.txOutValue validatingInput
          !scriptAddr          = V2.txOutAddress validatingInput
      in traceIfFalse "Sign" (signedBy txSigners walletPkh)              -- seller must sign it
      && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue) -- seller must get the UTxO
      && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)             -- single tx going in, no continue
    
    -- | Transform the auction bid.
    (Bidding ptd _ _, Transform) ->
      let !walletPkh       = ptPkh ptd
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !scriptAddr      = V2.txOutAddress validatingInput
          !contTxOutputs   = getScriptOutputs txOutputs scriptAddr
      in case getOutboundDatum contTxOutputs of
        -- transform back into the bidding state
        (Bidding ptd' _ _) -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
          && traceIfFalse "newo" (ptd == ptd')                   -- new owner must own it
          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1) -- single tx going in
          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out

        -- offer into a swappable
        (Swappable ptd' _ td') -> 
             traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, no continue
          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
          && traceIfFalse "owns" (ptd == ptd')                   -- seller cant change
          && traceIfFalse "Auct" (checkValidTimeData td')        -- valid auction time lock

        -- other datums fail
        _ -> traceIfFalse "Bidding:Transform:Undefined Datum" False
    
    -- | Complete an auction with a specific auction UTxO.
    (Bidding ptd mod pd, Complete) ->
      let !txId            = createTxOutRef (moTx mod) (moIdx mod)
          !info            = V2.scriptContextTxInfo context
          !txInputs        = V2.txInfoInputs info
          !txOutputs       = V2.txInfoOutputs info
          !txSigners       = V2.txInfoSignatories info
          !validatingInput = ownInput context
          !thisValue       = V2.txOutValue validatingInput
          !scriptAddr      = V2.txOutAddress validatingInput
          !contTxOutputs   = getScriptOutputs txOutputs scriptAddr
          !refTxIns        = V2.txInfoReferenceInputs info
          !refTxOut        = getReferenceInput refTxIns refHash
          !refDatum        = getReferenceDatum refTxOut
          !refValue        = V2.txOutValue refTxOut
      in case getDatumByTxId txId txInputs of
        -- auctioning only
        (Auctioning ptd' atd _) -> 
          let !sellerPkh           = ptPkh ptd'
              !sellerAddr          = UF.createAddress sellerPkh (ptSc ptd')
              !auctionTimeInterval = UF.lockBetweenTimeInterval (tStart atd) (tEnd atd)
              !txValidityRange     = V2.txInfoValidRange info
          in traceIfFalse "Sign" (signedBy txSigners sellerPkh)                                  -- seller must sign it
          && traceIfFalse "pays" (findPayout txOutputs sellerAddr thisValue)                     -- seller must get the UTxO
          && traceIfFalse "oldo" (ptd /= ptd')                                                   -- cant sell this to self
          && traceIfFalse "ins"  (nInputs txInputs scriptAddr 2)                                 -- double tx going in
          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)                                      -- single going out
          && traceIfFalse "Auct" (UF.isTxOutsideInterval auctionTimeInterval txValidityRange)    -- seller can unlock it
          && traceIfFalse "fee"  (checkServiceFeePayout (Bidding ptd mod pd) refDatum txOutputs) -- check if paying fee
          && traceIfFalse "val"  (Value.valueOf refValue lockPid lockTkn == 1)                   -- check if correct reference
            
        -- anything else fails
        _ -> traceIfFalse "Bidding:Complete:Undefined Datum" False
--DEBUG------------------------------------------------------------------------
    (_, _) -> traceIfFalse "Undefined State" False
--Functions--------------------------------------------------------------------
  where
    createTxOutRef :: V2.BuiltinByteString -> Integer -> V2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: V2.TxOutRef
        txId = V2.TxOutRef
          { V2.txOutRefId  = V2.TxId { V2.getTxId = txHash }
          , V2.txOutRefIdx = index
          }
    
    getReferenceDatum :: V2.TxOut -> ReferenceDatum
    getReferenceDatum x = 
      case V2.txOutDatum x of
        V2.NoOutputDatum              -> traceError "No Datum"
        (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @ReferenceDatum inline

    getOutboundDatumByValue :: [V2.TxOut] -> V2.Value -> CustomDatumType
    getOutboundDatumByValue txOuts val' = getOutboundDatumByValue' txOuts val'
      where
        getOutboundDatumByValue' :: [V2.TxOut] -> V2.Value -> CustomDatumType
        getOutboundDatumByValue' []     _   = traceError "Nothing Found"
        getOutboundDatumByValue' (x:xs) val =
          if V2.txOutValue x == val -- strict value continue
            then
              case V2.txOutDatum x of
                V2.NoOutputDatum              -> getOutboundDatumByValue' xs val -- skip datumless
                (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
                (V2.OutputDatum (V2.Datum d)) -> 
                  case PlutusTx.fromBuiltinData d of
                    Nothing     -> traceError "Bad Data"
                    Just inline -> PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
            else getOutboundDatumByValue' xs val
    
    getOutboundDatum :: [V2.TxOut] -> CustomDatumType
    getOutboundDatum txOuts = getOutboundDatum' txOuts
      where
        getOutboundDatum' :: [V2.TxOut] -> CustomDatumType
        getOutboundDatum' []     = traceError "Nothing Found"
        getOutboundDatum' (x:xs) =
          case V2.txOutDatum x of
            V2.NoOutputDatum              -> getOutboundDatum' xs
            (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
            (V2.OutputDatum (V2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data"
                Just inline -> PlutusTx.unsafeFromBuiltinData @CustomDatumType inline

    getDatumByTxId :: V2.TxOutRef -> [V2.TxInInfo] -> CustomDatumType
    getDatumByTxId txId txInputs = 
      case V2.txOutDatum $ V2.txInInfoResolved $ txInFromTxRef txInputs txId of
        V2.NoOutputDatum              -> traceError "No Datum"
        (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @CustomDatumType inline

-------------------------------------------------------------------------------
-- | Now we need to compile the validator.
-------------------------------------------------------------------------------
wrappedValidator :: ScriptParameters -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator s x y z = check (mkValidator s (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y) (V2.unsafeFromBuiltinData z))

validator :: ScriptParameters -> V2.Validator
validator sp = Plutonomy.optimizeUPLCWith theOptimizerOptions $ 
  Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $
  $$(PlutusTx.compile [|| wrappedValidator ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

swapContractScript :: ScriptParameters -> PlutusScript PlutusScriptV2
swapContractScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . validator