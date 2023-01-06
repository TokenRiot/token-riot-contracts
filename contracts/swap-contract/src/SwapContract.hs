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
module SwapContract
  ( swapContractScript
  , swapContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           SwappableDataType
import           AuctionDataType
import           UsefulFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType =  Swappable  PayToData PaymentData TimeData |
                        Auctioning AuctionData                    |
                        Offering   PayToData MakeOfferData
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ( 'Swappable,  0 )
                                              , ( 'Auctioning, 1 )
                                              , ( 'Offering,   2 )
                                              ]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove                                       |
                          FlatRate  PayToData ADAIncData SpecificToken |
                          Offer     ADAIncData MakeOfferData           |
                          SwapUTxO  PayToData                          |
                          Update    ADAIncData                         |
                          Bid       BidData                            |
                          Complete                                     |
                          OrderBook                                    |
                          Transform                                    |
                          FRRemove  PayToData SpecificToken            |
                          ORemove   MakeOfferData                      |
                          Debug
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove,    0 )
                                                , ( 'FlatRate,  1 )
                                                , ( 'Offer,     2 )
                                                , ( 'SwapUTxO,  3 )
                                                , ( 'Update,    4 )
                                                , ( 'Bid,       5 )
                                                , ( 'Complete,  6 )
                                                , ( 'OrderBook, 7 )
                                                , ( 'Transform, 8 )
                                                , ( 'FRRemove,  9 )
                                                , ( 'ORemove,  10 )
                                                , ( 'Debug,    11 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
    {- | Swappable SwappableData

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
    (Swappable ptd pd td) ->
      case redeemer of
        -- | A trader may transform their UTxO, holding the owner constant, changing the value and time.
        Transform -> 
          case getOutboundDatum contTxOutputs of
            Nothing            -> traceIfFalse "Swappable:Transform:GetOutboundDatum" False
            Just outboundDatum ->
              case outboundDatum of
                -- transform a swappable utxo
                (Swappable ptd' _ td') -> do
                  { let lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)               -- seller must sign it
                  ; let b = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1   -- single tx going in, single going out
                  ; let c = traceIfFalse "Datum Is Changing"   $ ptd == ptd'                                          -- seller cant change
                  ; let d = traceIfFalse "Invalid Time Change" $ checkValidTimeLock td td'                            -- valid time lock
                  ; let e = traceIfFalse "Time Lock Is Live"   $ isTxOutsideInterval lockTimeInterval txValidityRange -- seller can unlock it
                  ;         traceIfFalse "Swappable:Transform" $ all (==(True :: Bool)) [a,b,c,d,e]
                  }

                -- transform utxo into an offer
                (Offering ptd' _) -> do
                  { let lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)               -- seller must sign it
                  ; let b = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1   -- single tx going in, single going out
                  ; let c = traceIfFalse "Datum Is Changing"   $ ptd == ptd'                                          -- seller cant change
                  ; let d = traceIfFalse "Time Lock Is Live"   $ isTxOutsideInterval lockTimeInterval txValidityRange -- seller can unlock it
                  ;         traceIfFalse "Swappable:Transform" $ all (==(True :: Bool)) [a,b,c,d]
                  }
                -- other datums fail
                _ -> traceIfFalse "Swappable:Transform:Undefined Datum" False
        
        -- | A trader may update their UTxO, holding validating value constant, incrementing the min ada, and changing the payment datum.
        (Update aid) -> let incomingValue = validatingValue + adaValue (adaInc aid) in
          case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Swappable:Update:GetOutboundDatumByValue" False
            Just outboundDatum ->
              case outboundDatum of
                -- swap update
                (Swappable ptd' _ td') -> do
                  { let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)             -- seller must sign it
                  ; let b = traceIfFalse "Incorrect Datum"     $ (ptd == ptd') && (td == td')                       -- seller and time can't change
                  ; let c = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single tx going in, single going out
                  ;         traceIfFalse "Swappable:Update"    $ all (==(True :: Bool)) [a,b,c]
                  }

                -- A trader may update their UTxO into an auction.
                -- TODO
                (Auctioning _) -> False

                -- Other Datums fail
                _ -> traceIfFalse "Swappable:Update:Undefined Datum" False
        
        -- | A trader may remove their UTxO if not currently being timelocked.
        Remove -> do
          { let walletPkh        = ptPkh ptd
          ; let walletAddr       = createAddress walletPkh (ptSc ptd)
          ; let lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
          ; let txValidityRange  = ContextsV2.txInfoValidRange info
          ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info walletPkh                          -- seller must sign it
          ; let b = traceIfFalse "Value Not Returning" $ isAddrGettingPaidExactly txOutputs walletAddr validatingValue -- seller must get the UTxO
          ; let c = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0            -- single tx going in, no continue
          ; let d = traceIfFalse "Time Lock Is Live"   $ isTxOutsideInterval lockTimeInterval txValidityRange          -- seller can unlock it
          ;         traceIfFalse "Swappable:Remove"    $ all (==(True :: Bool)) [a,b,c,d]
          }
        
        -- | Order Book Dex Endpoint
        -- TODO
        OrderBook -> False

        -- | Swap ownership on two utxos with a multisig.
        -- TODO
        (SwapUTxO _) -> False

        -- | Flat rate swap of UTxO for an predefined amount of a single token.
        (FlatRate ptd' aid st) -> let incomingValue = validatingValue + adaValue (adaInc aid)
          in case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Swappable:FlatRate:getOutboundDatumByValue" False
            Just outboundDatum ->
              case outboundDatum of
                -- swappable only
                (Swappable ptd'' _ td') -> do
                  { let sellerAddr = createAddress (ptPkh ptd) (ptSc ptd)
                  ; let thisTkn    = getTokenName pd st
                  ; let a = traceIfFalse "Payment Not Made"    $ isAddrHoldingExactlyToken txOutputs sellerAddr (pPid pd) thisTkn (pAmt pd) -- seller must be paid
                  ; let b = traceIfFalse "Ownership Change"    $ (ptd /= ptd'') && (ptd' == ptd'') && (td == td')                           -- seller change but remain locked
                  ; let c = traceIfFalse "Empty Payment Value" $ (pAmt pd) /= 0                                                             -- seller must define price
                  ; let d = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1                         -- single tx going in, single going out
                  ; let e = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd')                                    -- buyer must sign
                  ;         traceIfFalse "Swappable:FlatRate"  $ all (==(True :: Bool)) [a,b,c,d,e]
                  }

                -- other datums fail
                _ -> traceIfFalse "Swappable:FlatRate:Undefined Datum" False
        
        -- | Flat rate purchase into buyer wallet of UTxO for an predefined amount of a single token.
        (FRRemove ptd' st) -> do
          { let sellerAddr       = createAddress (ptPkh ptd)  (ptSc ptd)
          ; let buyerAddr        = createAddress (ptPkh ptd') (ptSc ptd')
          ; let lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
          ; let txValidityRange  = ContextsV2.txInfoValidRange info
          ; let thisTkn          = getTokenName pd st
          ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd')                                    -- buyer must sign
          ; let b = traceIfFalse "Payment Not Paid"    $ isAddrHoldingExactlyToken txOutputs sellerAddr (pPid pd) thisTkn (pAmt pd) -- seller must be paid
          ; let c = traceIfFalse "Token Not Paid"      $ isAddrGettingPaidExactly txOutputs buyerAddr validatingValue               -- buyer must be paid
          ; let d = traceIfFalse "Empty Payment Value" $ (pAmt pd) /= 0                                                             -- seller must define price
          ; let e = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0                         -- single tx going in, no continue
          ; let f = traceIfFalse "Time Lock Is Live"   $ isTxOutsideInterval lockTimeInterval txValidityRange                       -- seller can unlock UTxO
          ;         traceIfFalse "Swappable:FRRemove"  $ all (==(True :: Bool)) [a,b,c,d,e,f]
          }

        -- | Offer to change walletship of UTxO for some amount of a single token + extras.
        (Offer aid mod) -> let txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            Nothing         -> traceIfFalse "Swappable:Offer:GetDatumByTxId" False
            Just otherDatum ->
              case otherDatum of
                -- offering only
                (Offering ptd' _) -> let incomingValue = validatingValue + adaValue (adaInc aid) in 
                  case getOutboundDatumByValue contTxOutputs incomingValue of
                    Nothing            -> traceIfFalse "Swappable:Offer:getOutboundDatumByValue" False
                    Just outboundDatum ->
                      case outboundDatum of
                        -- cont into swappable only
                        (Swappable ptd'' _ td') -> do
                          { let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)             -- seller must sign it
                          ; let b = traceIfFalse "Datum Equality"      $ (ptd /= ptd'') && (ptd' == ptd'') && (td == td')   -- seller change but remain locked
                          ; let c = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 2 && isNOutputs contTxOutputs 1 -- two tx going in, single going out
                          ;         traceIfFalse "Swappable:Offer"     $ all (==(True :: Bool)) [a,b,c]
                          }

                        -- other datums fail
                        _ -> traceIfFalse "Swappable:Offer:Undefined Datum" False
                  
                -- anything else fails
                _ -> traceIfFalse "Swappable:Offering:Undefined Datum" False
        
        -- | Offer but remove it to a the buyer's wallet
        (ORemove mod) -> let txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            Nothing         -> traceIfFalse "Swappable:OfferRemove:GetDatumByTxId" False
            Just otherDatum ->
              case otherDatum of
                -- offering only
                (Offering ptd' _) -> do
                  { let buyerAddr        = createAddress (ptPkh ptd') (ptSc ptd')
                  ; let lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)                       -- seller must sign it
                  ; let b = traceIfFalse "Token Not Paid"      $ isAddrGettingPaidExactly txOutputs buyerAddr validatingValue -- buyer must be paid
                  ; let c = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 2 && isNOutputs contTxOutputs 0           -- two tx going in, no continue
                  ; let d = traceIfFalse "Time Lock Is Live"   $ isTxOutsideInterval lockTimeInterval txValidityRange         -- seller can unlock it
                  ;         traceIfFalse "Swappable:ORemove"   $ all (==(True :: Bool)) [a,b,c,d]
                  }
                
                -- anything else fails
                _ -> traceIfFalse "Swappable:Offering:Undefined Datum" False

        -- | Other redeemers fail.
        _ -> traceIfFalse "Swappable:Undefined Redeemer" False
    
    {- | Offering OfferData

      Allows many users to store their offers inside the contract for some offer 
      trade to occur. An offerer may remove their current offers or transform their
      offer into a new offer. To ensure that a specific offer goes to a specific 
      UTxO, the TxID needs to attached of the UTxO an offerer is making an offer.

      The TxId in the Offering datum is the TxId of the UTxO that an offer is 
      being made on. If the TxId changes then the offerer will need to transform
      their offer to account for TxId change.

    -}
    (Offering ptd mod) ->
      case redeemer of
        -- | Remove the UTxO from the contract.
        Remove -> do
          { let walletPkh  = ptPkh ptd
          ; let walletAddr = createAddress walletPkh (ptSc ptd)
          ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info walletPkh                          -- wallet must sign it
          ; let b = traceIfFalse "Value Not Returning" $ isAddrGettingPaidExactly txOutputs walletAddr validatingValue -- wallet must get the UTxO
          ; let c = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0            -- single input no cont output
          ;         traceIfFalse "Offering:Remove"     $ all (==(True :: Bool)) [a,b,c]
          }
        
        -- | Transform the make offer tx ref info
        Transform -> 
          case getOutboundDatum contTxOutputs of
            Nothing            -> traceIfFalse "Offering:Transform:GetOutboundDatum" False
            Just outboundDatum ->
              case outboundDatum of
                -- offering only
                (Offering ptd' _) -> do
                  { let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)             -- wallet must sign it
                  ; let b = traceIfFalse "Incorrect Datum"     $ ptd == ptd'                                        -- wallet + stake can't change
                  ; let c = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single tx going in, single going out
                  ;         traceIfFalse "Offering:Transform"  $ all (==(True :: Bool)) [a,b,c]
                  }
                
                -- offer into a swappable
                (Swappable ptd' _ td') -> do
                  { let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info (ptPkh ptd)             -- seller must sign it
                  ; let b = traceIfFalse "Too Many In/Out"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single tx going in, single going out
                  ; let c = traceIfFalse "Datum Is Changing"   $ ptd == ptd'                                        -- seller cant change
                  ; let d = traceIfFalse "Invalid Time Change" $ checkValidTimeData td'                             -- valid time lock
                  ;         traceIfFalse "Swappable:Transform" $ all (==(True :: Bool)) [a,b,c,d,e]
                  }
                
                -- other endpoints fail
                _ -> False
        
        -- | Complete an offer with a specific swappable UTxO.
        Complete ->  let txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            Nothing         -> traceIfFalse "Offering:Complete:GetDatumByTxId" False
            Just otherDatum ->
              case otherDatum of
                -- swappable only
                (Swappable ptd' _ _) -> do
                  { let sellerPkh  = ptPkh ptd'
                  ; let sellerAddr = createAddress sellerPkh (ptSc ptd')
                  ; let a = traceIfFalse "Incorrect Tx Signer" $ ContextsV2.txSignedBy info sellerPkh                          -- The seller must sign it 
                  ; let b = traceIfFalse "Offer Not Returned"  $ isAddrGettingPaidExactly txOutputs sellerAddr validatingValue -- token must go back to printer
                  ; let c = traceIfFalse "Single Script UTxO"  $ isNInputs txInputs 2                                          -- single script input
                  ;         traceIfFalse "Offering:Complete"   $ all (==(True :: Bool)) [a,b,c]
                  }
                
                -- anything else fails
                _ -> False
        
        -- | Other Offering endpoints fail
        _ -> False

    {- | Auctioning AuctionData
      
      Allows a UTxO to be auctioned for some amount of time. Successful auctions are
      placed back into the swappable state as the auction state is a time lock. Similarly 
      to the swappable state, auctions remain in the contract after completion.

      The auction start time and end time are set in the same fashion as the time locking state.

      echo `expr $(echo $(date +%s%3N)) + $(echo 0)`
      # 1659817471786

      A five (5) minute window would be 5 * 60 * 1000  = 300,000.
      
      echo `expr $(echo $(date +%s%3N)) + $(echo 300000)`
      # 1659817771786

      TODO
    -}
    (Auctioning _) -> False
  --
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate."
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    getTokenName :: PaymentData -> SpecificToken -> PlutusV2.TokenName
    getTokenName pay tkn =
      if pAny pay == 0
        then pTkn pay
        else sTkn tkn

    -- Create a TxOutRef from the tx hash and index.
    createTxOutRef :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: PlutusV2.TxOutRef
        txId = PlutusV2.TxOutRef
          { PlutusV2.txOutRefId  = PlutusV2.TxId { PlutusV2.getTxId = txHash }
          , PlutusV2.txOutRefIdx = index
          }

    getOutboundDatumByValue :: [PlutusV2.TxOut] -> PlutusV2.Value -> Maybe CustomDatumType
    getOutboundDatumByValue []     _   = Nothing
    getOutboundDatumByValue (x:xs) val =
      if PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> getOutboundDatumByValue xs val -- skip datumless
            (PlutusV2.OutputDatumHash _) -> getOutboundDatumByValue xs val -- skip embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> getOutboundDatumByValue xs val
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
        else getOutboundDatumByValue xs val
    
    getOutboundDatum :: [PlutusV2.TxOut] -> Maybe CustomDatumType
    getOutboundDatum []     = Nothing
    getOutboundDatum (x:xs) =
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> getOutboundDatum xs -- skip datumless
        (PlutusV2.OutputDatumHash _) -> getOutboundDatum xs -- skip embedded datum
        -- inline datum only
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> getOutboundDatum xs
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline

    getDatumByTxId :: PlutusV2.TxOutRef -> Maybe CustomDatumType
    getDatumByTxId txId = 
      case ContextsV2.findTxInByTxOutRef txId info of
        Nothing -> Nothing
        Just txIn -> 
          case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved txIn of
            PlutusV2.NoOutputDatum       -> Nothing -- skip datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- skip embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

swapContractScriptShortBs :: SBS.ShortByteString
swapContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

swapContractScript :: PlutusScript PlutusScriptV2
swapContractScript = PlutusScriptSerialised swapContractScriptShortBs
