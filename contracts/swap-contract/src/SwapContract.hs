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
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           SwappableDataType
import           AuctionDataType
import           HelperFunctions
import           UsefulFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  
  cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
  git rev 950c4e222086fed5ca53564e642434ce9307b0b9

  cabal-install version 3.6.2.0
  compiled using version 3.6.2.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.7
-}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType =  Swappable SwappableData  |
                        Auctioning AuctionData
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ( 'Swappable,   0 )
                                              , ( 'Auctioning,  1 )
                                              ]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove                      |
                          FlatRate PayToData          |
                          Offer PayToData   |
                          SwapUTxO PayToData          |
                          Update PayToData            |
                          Bid BidData                 |
                          Complete                    |
                          OrderBook                   |
                          Transform                   |
                          FRRemove PayToData          |
                          ORemove PayToData |
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

      Different ways to swap utxo walletship.

      Redeemers will determine the type of swap being used. Unless the UTxO is being
      explicity removed, the UTxO is assumed to be continuing inside the contract.

      User's are allowed to update and remove their utxos at will.

      To Lock some UTxO for some pre-defined interval of time. The datum expects
      input integers that are greater than or equal to zero.

      echo `expr $(echo $(date +%s%3N)) + $(echo 0)`
      # 1659817471786

      A five (5) minute window would be 5 * 60 * 1000  = 300,000.
      
      echo `expr $(echo $(date +%s%3N)) + $(echo 300000)`
      # 1659817771786
    -}
    (Swappable sd) ->
      case redeemer of
        -- | A trader may transform their UTxO. Hold the datum constant and change the value.
        Transform -> 
          case getOutboundDatum contTxOutputs of
            Nothing            -> traceIfFalse "Swappable:Transform:GetOutboundDatum Error" False
            Just outboundDatum ->
              case outboundDatum of
                -- only can transform into the swap state
                (Swappable sd') -> do
                  { let lockTimeInterval = lockBetweenTimeInterval (sStart sd) (sEnd sd)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (sPkh sd)                 -- wallet must sign it
                  ; let b = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1   -- single tx going in
                  ; let c = traceIfFalse "Datum Is Changing Error"   $ sd == sd'                                            -- datum cant change
                  ; let d = traceIfFalse "Time Lock Is Live Error"   $ isTxOutsideInterval lockTimeInterval txValidityRange -- wallet can unlock it
                  ;         traceIfFalse "Swappable:Transform Error" $ all (==(True :: Bool)) [a,b,c,d]
                  }

                -- other datums fail
                _ -> traceIfFalse "Swappable:Transform:Undefined Datum Error" False
        
        -- | A trader may update their UTxO. Hold value constant, minus the increase, and change the datum.
        (Update ptd) -> let incomingValue = validatingValue + adaValue (pInc ptd) in
          case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Swappable:Update:GetOutboundDatumByValue Error" False
            Just outboundDatum ->
              case outboundDatum of
                -- swap update
                (Swappable sd') -> do
                  { let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (sPkh sd)               -- wallet must sign it
                  ; let b = traceIfFalse "Incorrect Datum Error"     $ checkIfTimeCanChange sd sd'                        -- wallet + stake can't change
                  ; let c = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single tx going in, single going out
                  ;         traceIfFalse "Swappable:Update Error"    $ all (==(True :: Bool)) [a,b,c]
                  }

                -- auction switch
                (Auctioning ad) -> do
                  { let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (sPkh sd)               -- wallet must sign it
                  ; let b = traceIfFalse "Datum Not Changing Error"  $ switchStates ad sd                                 -- wallet + stake can't change
                  ; let c = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single tx going in, single going out
                  ;         traceIfFalse "Swappable:Update Error"    $ all (==(True :: Bool)) [a,b,c]
                  }
        
        -- | A trader may remove their UTxO if not currently being timelocked.
        Remove -> do
          { let walletPkh        = sPkh sd
          ; let walletAddr       = createAddress walletPkh (sSc sd)
          ; let lockTimeInterval = lockBetweenTimeInterval (sStart sd) (sEnd sd)
          ; let txValidityRange  = ContextsV2.txInfoValidRange info
          ; let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info walletPkh                          -- wallet must sign it
          ; let b = traceIfFalse "Value Not Returning Error" $ isAddrGettingPaidExactly txOutputs walletAddr validatingValue -- wallet must get the utxo
          ; let c = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0            -- single tx going in
          ; let d = traceIfFalse "Time Lock Is Live Error"   $ isTxOutsideInterval lockTimeInterval txValidityRange          -- wallet can unlock it
          ;         traceIfFalse "Swappable:Remove Error"    $ all (==(True :: Bool)) [a,b,c,d]
          }
        
        -- | Swap ownership on two utxos like an order book. (No Partial Filling)
        -- TODO
        OrderBook ->
          case getOutboundDatumByValue contTxOutputs validatingValue of
            Nothing            -> traceIfFalse "Swappable:OrderBook:getOutboundDatumByValue Error" False
            Just outboundDatum ->
              case outboundDatum of
                (Swappable sd') -> do
                  { let a = traceIfFalse "Ownership Change Error"    $ ownershipSwapCheck sd sd'                                    -- ensure time lock and owner change
                  ; let b = traceIfFalse "Cant Find Other Owner"     $ checkCombineDatum sd sd'                                     -- ensure other input datum pkh is on this output datum
                  ; let c = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 2 && isNOutputs contTxOutputs 2           -- single tx going in, single going out
                  ; let d = traceIfFalse "Wrong Amount of Redeemers" $ isNRedeemers (toList $ ContextsV2.txInfoRedeemers info) == 2 -- two redeemers in offchain tx
                  ; let e = traceIfFalse "Value Not Swappable"       $ (sAmt sd /= 0 && sSlip sd /= 0) && checkIfInputIsHolding sd  -- slip is non zero and the other input has the token
                  ;         traceIfFalse "Swappable:OrderBook Error" $ all (==(True :: Bool)) [a,b,c,d,e]
                  }

                -- other datums fail
                _ -> traceIfFalse "Swappable:OrderBook:Undefined Datum Error" False

        -- | Swap ownership on two utxos with a multisig.
        -- TODO
        (SwapUTxO ptd) -> let incomingValue = validatingValue + adaValue (pInc ptd) in 
          case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Swappable:SwapUTxo:getOutboundDatumByValue Error" False
            Just outboundDatum ->
              case outboundDatum of
                (Swappable sd') -> do
                  { let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (sPkh sd)                         -- wallet must sign it
                  ; let b = traceIfFalse "Ownership Change Error"    $ ownershipSwapCheck sd sd'                                    -- wallet change but remain locked
                  ; let c = traceIfFalse "Cant Find Other Owner"     $ checkCombineDatum sd sd'                                     -- wallet change but remain locked
                  ; let d = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 2 && isNOutputs contTxOutputs 2           -- single tx going in, single going out
                  ; let e = traceIfFalse "Wrong Amount of Redeemers" $ isNRedeemers (toList $ ContextsV2.txInfoRedeemers info) == 2 -- two redeemers in offchain tx
                  ;         traceIfFalse "Swappable:SwapUTxo Error"  $ all (==(True :: Bool)) [a,b,c,d,e]
                  }

                -- other datums fail
                _ -> traceIfFalse "Swappable:SwapUTxo:Undefined Datum Error" False

        -- | Flat rate walletship swap of utxo for an predefined amount of a single token.
        (FlatRate ptd) -> let incomingValue = validatingValue + adaValue (pInc ptd) in 
          case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Swappable:FlatRate:getOutboundDatumByValue Error" False
            Just outboundDatum ->
              case outboundDatum of
                (Swappable sd') -> do
                  { let walletAddr = createAddress (sPkh sd) (sSc sd)
                  ; let a = traceIfFalse "Payment Not Made Error"    $ isAddrHoldingExactlyToken txOutputs walletAddr (sPid sd) (sTkn sd) (sAmt sd) -- wallet must be paid
                  ; let b = traceIfFalse "Ownership Change Error"    $ ownershipSwapCheck sd sd' && proveOwnership ptd sd'                          -- wallet change but remain locked
                  ; let c = traceIfFalse "Empty Payment Value Error" $ (sAmt sd) /= 0                                                               -- wallet must define price
                  ; let d = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1                           -- single tx going in, single going out
                  ; let e = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (ptPkh ptd)                                       -- buyer must sign
                  ;         traceIfFalse "Swappable:FlatRate Error"  $ all (==(True :: Bool)) [a,b,c,d,e]
                  }

                -- other datums fail
                _ -> traceIfFalse "Swappable:FlatRate:Undefined Datum Error" False
        
        -- | Flat rate purchase into buyer wallet of utxo for an predefined amount of a single token.
        (FRRemove ptd) -> do
          { let sellerAddr       = createAddress (sPkh sd)   (sSc sd)
          ; let buyerAddr        = createAddress (ptPkh ptd) (ptSc ptd)
          ; let lockTimeInterval = lockBetweenTimeInterval (sStart sd) (sEnd sd)
          ; let txValidityRange  = ContextsV2.txInfoValidRange info
          ; let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (ptPkh ptd)                                       -- buyer must be paid
          ; let b = traceIfFalse "Payment Not Paid Error"    $ isAddrHoldingExactlyToken txOutputs sellerAddr (sPid sd) (sTkn sd) (sAmt sd) -- seller must be paid
          ; let c = traceIfFalse "Token Not Paid Error"      $ isAddrGettingPaidExactly txOutputs buyerAddr validatingValue                 -- buyer must be paid
          ; let d = traceIfFalse "Empty Payment Value Error" $ (sAmt sd) /= 0                                                               -- wallet must define price
          ; let e = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0                           -- single tx going in, single going out
          ; let f = traceIfFalse "Time Lock Is Live Error"   $ isTxOutsideInterval lockTimeInterval txValidityRange                         -- wallet can unlock it
          ;         traceIfFalse "Swappable:FRRemove Error"  $ all (==(True :: Bool)) [a,b,c,d,e,f]
          }

        -- | Offer to change walletship of utxo for some amount of a single token + extras.
        (Offer ptd) -> let incomingValue = validatingValue + adaValue (pInc ptd) in 
          case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Swappable:Offer:getOutboundDatumByValue Error" False
            Just outboundDatum ->
              case outboundDatum of
                (Swappable sd') -> do
                  { let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (sPkh sd)                -- seller must sign it
                  ; let b = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (ptPkh ptd)              -- buyer must sign it
                  ; let c = traceIfFalse "Datum Equality Error"      $ ownershipSwapCheck sd sd' && proveOwnership ptd sd' -- wallet change but remain locked
                  ; let d = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1  -- single tx going in, single going out
                  ;         traceIfFalse "Swappable:Offer Error"     $ all (==(True :: Bool)) [a,b,c,d]
                  }

                -- other datums fail
                _ -> traceIfFalse "Swappable:Offer:Undefined Datum Error" False
        
        -- | Offer but remove it to a the buyer's wallet
        (ORemove ptd) -> do
          { let buyerAddr        = createAddress (ptPkh ptd) (ptSc ptd)
          ; let lockTimeInterval = lockBetweenTimeInterval (sStart sd) (sEnd sd)
          ; let txValidityRange  = ContextsV2.txInfoValidRange info
          ; let a = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (sPkh sd)                         -- seller must sign it
          ; let b = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info (ptPkh ptd)                       -- buyer must sign it
          ; let c = traceIfFalse "Token Not Paid Error"      $ isAddrGettingPaidExactly txOutputs buyerAddr validatingValue -- buyer must be paid
          ; let d = traceIfFalse "Too Many In/Out Error"     $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0           -- single tx going in, single going out
          ; let e = traceIfFalse "Time Lock Is Live Error"   $ isTxOutsideInterval lockTimeInterval txValidityRange         -- wallet can unlock it
          ;         traceIfFalse "Swappable:ORemove Error"   $ all (==(True :: Bool)) [a,b,c,d,e]
          }

        -- | Other redeemers fail.
        _ -> traceIfFalse "Swappable:Undefined Redeemer Error" False

    {- |Auctioning AuctionData
      
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
    (Auctioning ad) -> let currentBid = Value.singleton (aPid ad) (aTkn ad) (aPrice ad) in -- currentbid value
      case redeemer of
        -- | Remove the UTxO from the contract before the auction starts or after a failed auction.
        Remove -> do
          { let walletPkh  = aSellerPkh ad
          ; let walletAddr = createAddress walletPkh (aSellerSc ad)
          ; let lockTimeInterval = lockBetweenTimeInterval (aLockStart ad) (aLockEnd ad)
          ; let auctionTimeInterval = lockBetweenTimeInterval (aStartTime ad) (aEndTime ad)
          ; let txValidityRange  = ContextsV2.txInfoValidRange info
          ; let a = traceIfFalse "Incorrect Tx Signer Error"  $ (aPrice ad) <= (aMinimum ad) && ContextsV2.txSignedBy info walletPkh -- no bids and wallet must sign it
          ; let b = traceIfFalse "Value Not Returning Error"  $ isAddrGettingPaidExactly txOutputs walletAddr validatingValue        -- wallet must get the utxo
          ; let c = traceIfFalse "Time Lock Is Live Error"    $ isTxOutsideInterval lockTimeInterval txValidityRange                 -- wallet can unlock it
          ; let d = traceIfFalse "Auction Lock Is Live Error" $ isTxOutsideInterval auctionTimeInterval txValidityRange              -- a wallet can unlock it
          ; let e = traceIfFalse "Too Many In/Out Error"      $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0                   -- single input no cont output
          ;         traceIfFalse "Auctioning:Remove Error"    $ all (==(True :: Bool)) [a,b,c,d,e]
          }
        
        -- | Update that allows going back to the auctioning data or the swappable data.
        (Update ptd) ->  let incomingValue = validatingValue + adaValue (pInc ptd) in 
          case getOutboundDatumByValue contTxOutputs incomingValue of
            Nothing            -> traceIfFalse "Auctioning:Update:GetOutboundDatum Error" False
            Just outboundDatum ->
              case outboundDatum of
                -- go back to the swap state
                (Swappable sd) -> do
                  { let walletPkh  = aSellerPkh ad
                  ; let auctionTimeInterval = lockBetweenTimeInterval (aStartTime ad) (aEndTime ad)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer Error"  $ (aPrice ad) <= (aMinimum ad) && ContextsV2.txSignedBy info walletPkh -- no bids and wallet must sign it
                  ; let b = traceIfFalse "Auction Lock Is Live Error" $ isTxOutsideInterval auctionTimeInterval txValidityRange              -- a wallet can unlock it
                  ; let c = traceIfFalse "Datum Equality Error"       $ switchStates ad sd                                                   -- this retains walletship of the utxo
                  ; let d = traceIfFalse "Too Many In/Out Error"      $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1                   -- single input no cont output
                  ;         traceIfFalse "Auctioning:Update:Swappable Error" $ all (==(True :: Bool)) [a,b,c,d]
                  }
                
                -- update current auction
                (Auctioning ad') -> do
                  { let walletPkh  = aSellerPkh ad
                  ; let auctionTimeInterval = lockBetweenTimeInterval (aStartTime ad) (aEndTime ad)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer Error"  $ (aPrice ad) <= (aMinimum ad) && ContextsV2.txSignedBy info walletPkh -- no bids and wallet must sign it
                  ; let b = traceIfFalse "Auction Lock Is Live Error" $ isTxOutsideInterval auctionTimeInterval txValidityRange              -- a wallet can unlock it
                  ; let c = traceIfFalse "Datum Update Error"         $ resetAuction ad ad'                                                  -- This allows updating auction data
                  ;         traceIfFalse "Auctioning:Update:Auctioning Error" $ all (==(True :: Bool)) [a,b,c]
                  }

        -- | Make a Bid on an ongoing auction.
        (Bid bd) -> let nextBid = Value.singleton (aPid ad) (aTkn ad) (bAmt bd) in  -- current next bid value
          case getOutboundDatumByValue contTxOutputs (validatingValue - currentBid + nextBid) of
            Nothing            -> traceIfFalse "Auction:Bid:GetOutboundDatum Error" False
            Just outboundDatum ->
              case outboundDatum of
                -- go back to the auction state
                (Auctioning ad') -> do
                  { let bidderAddr = createAddress (aBidderPkh ad) (aSellerSc ad)
                  ; let auctionTimeInterval = lockBetweenTimeInterval (aStartTime ad) (aEndTime ad)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "The Auction Is Not Live" $ isTxInsideInterval auctionTimeInterval txValidityRange                                 -- must be inside the auction range
                  ; let b = traceIfFalse "Old Bid Not Returning"   $ (aPrice ad <= aMinimum ad) || isAddrGettingPaidExactly txOutputs bidderAddr currentBid -- pass if no bid else send back old bid
                  ; let c = traceIfFalse "Bid Must Increase Price" $ bAmt bd == aPrice ad'                                                                  -- must equal the value inside the new datum
                  ; let d = traceIfFalse "Datum Update Error"      $ auctionBidCheck ad ad'                                                                 -- this applies the bid equality
                  ;         traceIfFalse "Auctioning:Bid:Auctioning Error" $ all (==True) [a,b,c,d]
                  }
                -- other datums fail
                _ -> traceIfFalse "Auctioning:Bid:Undefined Datum Error" False
        
        -- | Complete a finished auction.
        Complete ->
          case getOutboundDatumByValue contTxOutputs (validatingValue - currentBid) of
            Nothing            -> traceIfFalse "Auctioning:Complete:GetOutboundDatum Error" False
            Just outboundDatum ->
              case outboundDatum of
                -- go back to the swap state
                (Swappable sd) -> do
                  { let walletPkh  = aSellerPkh ad
                  ; let walletAddr = createAddress walletPkh (aSellerSc ad)
                  ; let auctionTimeInterval = lockBetweenTimeInterval (aStartTime ad) (aEndTime ad)
                  ; let txValidityRange  = ContextsV2.txInfoValidRange info
                  ; let a = traceIfFalse "Incorrect Tx Signer Error" $ (aPrice ad) > (aMinimum ad)                              -- someone had to bid
                  ; let b = traceIfFalse "Time Lock Is Live Error"   $ isTxOutsideInterval auctionTimeInterval txValidityRange  -- not time locked
                  ; let c = traceIfFalse "Datum Equality Error"      $ successfulAuction ad sd                                  -- datum changes correctly
                  ; let d = traceIfFalse "Correct wallet Error"      $ aBidderPkh ad == sPkh sd                                 -- correct walletship change
                  ; let e = traceIfFalse "Bid Not Returning"         $ isAddrGettingPaidExactly txOutputs walletAddr currentBid -- wallet gets paid
                  ;         traceIfFalse "Auctioning:Complete:Swappable Error" $ all (==(True :: Bool)) [a,b,c,d,e]
                  }
                
                -- other datums fail
                _ -> traceIfFalse "Auctioning:Complete:Undefined Datum Error" False
        
        -- | Other redeemers fail.
        _ -> traceIfFalse "Auctioning:Undefined Redeemer Error" False
  --
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    -- inputs outputs
    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    -- what is currently being spent
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- if time locked, update sale and only extend lock else update sale and timelock
    checkIfTimeCanChange :: SwappableData -> SwappableData -> Bool
    checkIfTimeCanChange sd sd' = (       isTxOutsideInterval lockTimeInterval txValidityRange && priceUpdateWithTimeCheck sd sd' ) || -- is not time locked
                                  ( not $ isTxOutsideInterval lockTimeInterval txValidityRange && priceUpdateCheck         sd sd' )    -- is time locked
      where
        lockTimeInterval :: PlutusV2.Interval PlutusV2.POSIXTime
        lockTimeInterval = lockBetweenTimeInterval (sStart sd) (sEnd sd)

        txValidityRange :: PlutusV2.POSIXTimeRange
        txValidityRange = ContextsV2.txInfoValidRange info

    checkIfInputIsHolding :: SwappableData -> Bool
    checkIfInputIsHolding inDatum =
      case getOtherInputValue txInputs inDatum of
        Nothing         -> traceIfFalse "cant find input value" False
        Just otherValue -> traceIfFalse "value equality error" $ isValueInRange (sAmt inDatum) (sSlip inDatum) (target otherValue)
      where
        target :: PlutusV2.Value -> Integer
        target otherValue = Value.valueOf otherValue (sPid inDatum) (sTkn inDatum)

    getOtherInputValue :: [PlutusV2.TxInInfo] -> SwappableData -> Maybe PlutusV2.Value
    getOtherInputValue utxos sData = loopInputs utxos
      where
        loopInputs :: [PlutusV2.TxInInfo] -> Maybe PlutusV2.Value
        loopInputs []     = Nothing
        loopInputs (x:xs) =
          case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
            PlutusV2.NoOutputDatum       -> loopInputs xs
            (PlutusV2.OutputDatumHash _) -> loopInputs xs
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->
              case PlutusTx.fromBuiltinData d of
                Nothing     -> loopInputs xs
                Just inline -> let dataObj = PlutusTx.unsafeFromBuiltinData @CustomDatumType inline in
                  case dataObj of
                    (Swappable sData') ->
                      if sPkh sData /= sPkh sData'
                        then Just $ PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x
                        else loopInputs xs
                    _  -> loopInputs xs

    checkCombineDatum :: SwappableData -> SwappableData -> Bool
    checkCombineDatum inDatum outDatum =
      case getOtherInputDatum txInputs inDatum of
        Nothing         -> traceIfFalse "cant find input datum" False
        Just otherDatum -> traceIfFalse "pkh equality error" $ sPkh outDatum == sPkh otherDatum
        
    getOtherInputDatum :: [PlutusV2.TxInInfo] -> SwappableData -> Maybe SwappableData
    getOtherInputDatum utxos sData = loopInputs utxos
      where
        loopInputs :: [PlutusV2.TxInInfo] -> Maybe SwappableData
        loopInputs []     = Nothing
        loopInputs (x:xs) =
          case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
            PlutusV2.NoOutputDatum       -> loopInputs xs
            (PlutusV2.OutputDatumHash _) -> loopInputs xs
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->
              case PlutusTx.fromBuiltinData d of
                Nothing     -> loopInputs xs
                Just inline -> let dataObj = PlutusTx.unsafeFromBuiltinData @CustomDatumType inline in
                  case dataObj of
                    (Swappable sData') ->
                      if sPkh sData /= sPkh sData'
                        then Just sData'
                        else loopInputs xs
                    _  -> loopInputs xs

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
  --
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
