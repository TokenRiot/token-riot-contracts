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
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as V2
import qualified Plutus.V2.Ledger.Contexts      as V2
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import           SwappableDataType
import           ReferenceDataType
import           UsefulFuncs
import           ReducedFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
lockPid :: V2.CurrencySymbol
lockPid = V2.CurrencySymbol {V2.unCurrencySymbol = createBuiltinByteString [246, 241, 20, 169, 241, 31, 249, 131, 236, 10, 81, 220, 165, 251, 110, 182, 228, 253, 59, 3, 251, 143, 239, 16, 62, 130, 91, 119] }

lockTkn :: V2.TokenName
lockTkn = V2.TokenName {V2.unTokenName = createBuiltinByteString [84, 104, 105, 115, 73, 115, 79, 110, 101, 83, 116, 97, 114, 116, 101, 114, 84, 111, 107, 101, 110, 70, 111, 114, 84, 101, 115, 116, 105, 110, 103, 52] }

-- check for nft here
lockValue :: V2.Value
lockValue = Value.singleton lockPid lockTkn (1 :: Integer)

-- reference hash
referenceHash :: V2.ValidatorHash
referenceHash = V2.ValidatorHash $ createBuiltinByteString [129, 40, 202, 155, 192, 213, 103, 180, 68, 142, 122, 240, 14, 189, 69, 55, 164, 112, 237, 76, 227, 210, 47, 46, 37, 170, 139, 80]

{-# INLINABLE calculateServiceFee #-}
calculateServiceFee :: CustomDatumType -> ReferenceDatum -> Integer
calculateServiceFee (Swappable _ pd _) (Reference _ sf _) =
  if (pPid pd == Value.adaSymbol) && (pTkn pd == Value.adaToken)
  then if percentFee > sumFee then percentFee else sumFee
  else sumFee
  where
    percentFee :: Integer
    percentFee = divide (pAmt pd) (servicePerc sf)

    sumFee :: Integer
    sumFee = (serviceFee sf + frontendFee sf)
calculateServiceFee _ (Reference _ sf _) = (serviceFee sf + frontendFee sf)
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType
  = Swappable  PayToData PaymentData   TimeData
  | Auctioning PayToData TimeData      TimeData
  | Offering   PayToData MakeOfferData OfferFlagData
  | Bidding    PayToData MakeOfferData
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
  | FlatRate  PayToData ADAIncData SpecificToken
  | Offer     ADAIncData MakeOfferData
  | SwapUTxO  ADAIncData MakeOfferData
  | Update    ADAIncData
  | Bid
  | Complete
  | OrderBook
  | Transform
  | FRRemove  PayToData SpecificToken
  | ORemove   MakeOfferData
  | Debug
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
mkValidator :: CustomDatumType -> CustomRedeemerType -> V2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
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
    (Swappable ptd pd td) -> let !walletPkh        = ptPkh ptd
                                 !walletAddr       = createAddress walletPkh (ptSc ptd)
                                 !lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
                                 !txValidityRange  = V2.txInfoValidRange info
                                 !txSigners        = V2.txInfoSignatories info
      in case redeemer of
        -- | A trader may transform their UTxO, holding the owner constant, changing the value and time.
        Transform -> 
          case getOutboundDatum contTxOutputs of
            -- transform a swappable utxo
            (Swappable ptd' _ td') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                         -- seller must sign it
                                   && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                        -- single tx going in
                                   && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                             -- single going out
                                   && traceIfFalse "owns" (ptd == ptd')                                          -- seller cant change
                                   && traceIfFalse "time" (checkValidTimeLock td td')                            -- valid time lock
                                   && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it

            -- transform utxo into an offer
            (Offering ptd' _ _) -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                         -- seller must sign it
                                && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                        -- single tx going in
                                && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                             -- single going out
                                && traceIfFalse "owns" (ptd == ptd')                                          -- seller cant change
                                && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
            
            -- transform a swappable state into the auctioning state
            (Auctioning ptd' atd td') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                         -- seller must sign it
                                      && traceIfFalse "owns" (ptd == ptd')                                          -- seller cant change
                                      && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                        -- single tx going in
                                      && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                             -- single going out
                                      && traceIfFalse "Auct" (checkValidTimeData atd)                               -- valid auction time lock
                                      && traceIfFalse "time" (checkValidTimeLock td td')                            -- valid time lock
                                      && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
            
            -- transform a swappable state into a bid
            (Bidding ptd' _) -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                         -- seller must sign it
                             && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1)                        -- single tx going in
                             && traceIfFalse "Outs" (nOutputs contTxOutputs 1)                             -- single going out
                             && traceIfFalse "owns" (ptd == ptd')                                          -- seller cant change
                             && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
                
        -- | A trader may update their UTxO, holding validating value constant, incrementing the min ada, and changing the payment datum.
        (Update aid) -> let !incomingValue = thisValue + adaValue (adaInc aid) in
          case getOutboundDatumByValue contTxOutputs incomingValue of
            -- update the payment data on a swappable state
            (Swappable ptd' _ td') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
                                   && traceIfFalse "Datu" (ptd == ptd')                   -- seller and time can't change
                                   && traceIfFalse "Time" (td == td')                     -- seller and time can't change
                                   && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, single going out
                                   && traceIfFalse "Outs" (nOutputs contTxOutputs 1)      -- single tx going in, single going out

            -- Update a swappable state into the auctioning state
            (Auctioning ptd' atd td') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
                                      && traceIfFalse "Datu" (ptd == ptd')                   -- seller can't change
                                      && traceIfFalse "Time" (td == td')                     -- time can't change
                                      && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1) -- single tx going in
                                      && traceIfFalse "Outs" (nOutputs contTxOutputs 1)      -- single going out
                                      && traceIfFalse "Chan" (checkValidTimeData atd)        -- valid time lock

            -- Other Datums fail
            _ -> traceIfFalse "Swappable:Update:Undefined Datum" False
        
        -- | A trader may remove their UTxO if not currently being timelocked.
        Remove -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                         -- seller must sign it
               && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue)            -- seller must get the UTxO
               && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                        -- single tx going in, no continue
               && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
        
        -- | Order Book Dex Endpoint
        -- TODO
        OrderBook -> False

        -- | Swap ownership on two utxos with a multisig.
        -- TODO
        (SwapUTxO _ _) -> False

        -- | Flat rate swap of UTxO for an predefined amount of a single token.
        (FlatRate ptd' aid st) -> 
          let !incomingValue = thisValue + adaValue (adaInc aid)
              !thisTkn       = getTokenName pd st
              !refTxIns      = V2.txInfoReferenceInputs info
              !refTxOut      = getReferenceInput refTxIns referenceHash
              !refDatum      = getReferenceDatum refTxOut
              !refValue      = V2.txOutValue refTxOut
          in case getOutboundDatumByValue contTxOutputs incomingValue of
            -- swappable only
            (Swappable ptd'' _ td') -> traceIfFalse "Pays" (findTokenHolder txOutputs walletAddr (pPid pd) thisTkn (pAmt pd)) -- seller must be paid
                                    && traceIfFalse "oldo" (ptd /= ptd'')                                                     -- cant sell this
                                    && traceIfFalse "newo" (ptd' == ptd'')                                                    -- new owner must own it
                                    && traceIfFalse "time" (td == td')                                                        -- time data must remain
                                    && traceIfFalse "Empt" (pAmt pd /= 0)                                                     -- seller must define price
                                    && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                                    -- single tx going in
                                    && traceIfFalse "outs" (nOutputs contTxOutputs 1)                                         -- single going out
                                    && traceIfFalse "sign" (signedBy txSigners (ptPkh ptd'))                                  -- buyer must sign
                                    && traceIfFalse "val"  (Value.geq refValue lockValue)                                     -- check if correct reference
                                    && traceIfFalse "fee"  (checkServiceFeePayout (Swappable ptd pd td) refDatum)             -- check if paying fee

            -- other datums fail
            _ -> traceIfFalse "Swappable:FlatRate:Undefined Datum" False
        
        -- | Flat rate purchase into buyer wallet of UTxO for an predefined amount of a single token.
        (FRRemove ptd' st) -> 
          let !buyerPkh  = ptPkh ptd'
              !buyerAddr = createAddress buyerPkh (ptSc ptd')
              !thisTkn   = getTokenName pd st
              !refTxIns  = V2.txInfoReferenceInputs info
              !refTxOut  = getReferenceInput refTxIns referenceHash
              !refDatum  = getReferenceDatum refTxOut
              !refValue  = V2.txOutValue refTxOut
          in traceIfFalse "Signer" (signedBy txSigners buyerPkh)                                      -- seller must sign it
          && traceIfFalse "Tokens" (findTokenHolder txOutputs walletAddr (pPid pd) thisTkn (pAmt pd)) -- seller must be paid
          && traceIfFalse "Pays"   (findPayout txOutputs buyerAddr thisValue)                         -- buyer must be paid
          && traceIfFalse "Empty"  (pAmt pd /= 0)                                                     -- seller must define price
          && traceIfFalse "Ins"    (nInputs txInputs scriptAddr 1)                                    -- single tx going in
          && traceIfFalse "Lock"   (isTxOutsideInterval lockTimeInterval txValidityRange)             -- seller can unlock it
          && traceIfFalse "val"    (Value.geq refValue lockValue)                                     -- check if correct reference
          && traceIfFalse "fee"    (checkServiceFeePayout (Swappable ptd pd td) refDatum)             -- check if paying fee
        
        -- | Offer to change walletship of UTxO for some amount of a single token + extras.
        (Offer aid mod) -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- offering only
            (Offering ptd' _ ofd) -> let !incomingValue = thisValue + adaValue (adaInc aid) in 
              case getOutboundDatumByValue contTxOutputs incomingValue of
                -- cont into swappable only
                (Swappable ptd'' pd' td') -> traceIfFalse "sign" (signedBy txSigners walletPkh)  -- seller must sign
                                          && traceIfFalse "oldo" (ptd /= ptd'')                  -- cant sell this to self
                                          && traceIfFalse "newo" (ptd' == ptd'')                 -- new owner must own it
                                          && traceIfFalse "time" (td == td')                     -- time data must remain
                                          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 2) -- single tx going in
                                          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
                                          && traceIfFalse "PayD" (pd' == defaultPayment)         -- payment data must be default
                                          && traceIfFalse "Flag" (oFlag ofd == 0)                -- Offer stays in contract

                -- other datums fail
                _ -> traceIfFalse "Swappable:Offer:Undefined Datum" False
            
            -- anything else fails
            _ -> traceIfFalse "Swappable:Offering:Undefined Datum" False
        
        -- | Offer but remove it to a the buyer's wallet
        (ORemove mod) -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- offering only
            (Offering ptd' _ ofd) -> let !buyerAddr = createAddress (ptPkh ptd') (ptSc ptd')
                                  in traceIfFalse "sign" (signedBy txSigners walletPkh)                         -- seller must sign
                                  && traceIfFalse "oldo" (ptd /= ptd')                                          -- cant sell this to self
                                  && traceIfFalse "Pays" (findPayout txOutputs buyerAddr thisValue)             -- buyer must be paid
                                  && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 2)                        -- double tx going in
                                  && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
                                  && traceIfFalse "Flag" (oFlag ofd /= 0)                                       -- Offer stays in contract
            
            -- anything else fails
            _ -> traceIfFalse "Swappable:Offering:Undefined Datum" False

        -- | Other redeemers fail.
        _ -> traceIfFalse "Swappable:Undefined Redeemer" False
    
    {- | Offering State

      Allows many users to store their offers inside the contract for some offer 
      trade to occur. An offerer may remove their current offers or transform their
      offer into a new offer. To ensure that a specific offer goes to a specific 
      UTxO, the TxID needs to attached of the UTxO an offerer is making an offer.

      The TxId in the Offering datum is the TxId of the UTxO that an offer is 
      being made on. If the TxId changes then the offerer will need to transform
      their offer to account for TxId change.

    -}
    (Offering ptd mod _) -> let !walletPkh  = ptPkh ptd
                                !walletAddr = createAddress walletPkh (ptSc ptd)
                                !txSigners  = V2.txInfoSignatories info
      in case redeemer of
        -- | Remove the UTxO from the contract.
        Remove -> traceIfFalse "Sign" (signedBy txSigners walletPkh)              -- seller must sign it
               && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue) -- seller must get the UTxO
               && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)             -- single tx going in, no continue
        
        -- | Transform the make offer tx ref info
        Transform -> 
          case getOutboundDatum contTxOutputs of
            -- offering only
            (Offering ptd' _ _) -> traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
                                && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, no continue
                                && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
                                && traceIfFalse "owns" (ptd == ptd')                   -- seller cant change

            -- offer into a swappable
            (Swappable ptd' _ td') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
                                   && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, no continue
                                   && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
                                   && traceIfFalse "owns" (ptd == ptd')                   -- seller cant change
                                   && traceIfFalse "Auct" (checkValidTimeData td')        -- valid auction time lock
            
            -- other endpoints fail
            _ -> traceIfFalse "Offering:Transform:Undefined Datum" False
        
        -- | Complete an offer with a specific swappable UTxO.
        Complete ->  let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- swappable only
            (Swappable ptd' _ _) -> let !sellerPkh  = ptPkh ptd'
                                        !sellerAddr = createAddress sellerPkh (ptSc ptd')
                                 in traceIfFalse "Sign" (signedBy txSigners sellerPkh)              -- seller must sign it
                                 && traceIfFalse "oldo" (ptd /= ptd')                               -- cant sell this to self
                                 && traceIfFalse "pays" (findPayout txOutputs sellerAddr thisValue) -- seller must get the UTxO
                                 && traceIfFalse "ins"  (nInputs txInputs scriptAddr 2)             -- double tx going in
            
            -- anything else fails
            _ -> traceIfFalse "Offering:Complete:Undefined Datum" False
        
        -- | Other Offering endpoints fail
        _ -> traceIfFalse "Offering:Undefined Redeemer" False

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
    (Auctioning ptd atd gtd) -> let !walletPkh           = ptPkh ptd
                                    !walletAddr          = createAddress walletPkh (ptSc ptd)
                                    !lockTimeInterval    = lockBetweenTimeInterval (tStart gtd) (tEnd gtd)
                                    !auctionTimeInterval = lockBetweenTimeInterval (tStart atd) (tEnd atd)
                                    !txValidityRange     = V2.txInfoValidRange info
                                    !txSigners           = V2.txInfoSignatories info
      in case redeemer of
        -- | Remove the UTxO from the contract before the auction starts or after a failed auction.
        Remove -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
               && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue)               -- seller must get the UTxO
               && traceIfFalse "Lock" (isTxOutsideInterval lockTimeInterval txValidityRange)    -- seller can unlock it
               && traceIfFalse "Auct" (isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it
               && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in, no continue
        
        -- | Update the auction back into the swappable state.
        (Update aid) -> let !incomingValue = thisValue + adaValue (adaInc aid) in 
          case getOutboundDatumByValue contTxOutputs incomingValue of
            -- go back to the swap state
            (Swappable ptd' _ gtd') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)                            -- seller must sign it
                                    && traceIfFalse "Auct" (isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it
                                    && traceIfFalse "newo" (ptd == ptd')                                             -- new owner must own it
                                    && traceIfFalse "time" (gtd == gtd')                                             -- time data must remain
                                    && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)                           -- single tx going in, no continue
                                    && traceIfFalse "Out"  (nOutputs contTxOutputs 1)                                -- single going out
                            
            -- anything else fails
            _ -> traceIfFalse "Auctioning:Update:Undefined Datum" False

        -- | Offer the auction for some selected bid
        (Offer aid mod) -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- offering only
            (Bidding ptd' _) -> let !incomingValue = thisValue + adaValue (adaInc aid) in 
              case getOutboundDatumByValue contTxOutputs incomingValue of
                -- cont into swappable only
                (Swappable ptd'' pd' td') -> traceIfFalse "sign" (signedBy txSigners walletPkh)                            -- seller must sign
                                          && traceIfFalse "oldo" (ptd /= ptd'')                                            -- cant sell this to self
                                          && traceIfFalse "newo" (ptd' == ptd'')                                           -- new owner must own it
                                          && traceIfFalse "time" (gtd == td')                                              -- time data must remain
                                          && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 2)                           -- double tx going in
                                          && traceIfFalse "Out"  (nOutputs contTxOutputs 1)                                -- single going out
                                          && traceIfFalse "PayD" (pd' == defaultPayment)                                   -- payment data must be default
                                          && traceIfFalse "Auct" (isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it

                -- other datums fail
                _ -> traceIfFalse "Auctioning:Offer:Undefined Datum" False
            
            -- anything else fails
            _ -> traceIfFalse "Auctioning:Offering:Undefined Datum" False

        -- Other redeemers fail
        _ -> traceIfFalse "Auctioning:Undefined Redeemer" False
    
    {- | Bidding State 

      Allows a bidder to place their bid into the contract for an on going auction. At the end of
      the auction, the auctioner will select the best bid and complete the auction or they may reject
      all bids and remove the UTxO back into the swap state. Similar to the offer state, bidders will
      need to remove old bids when the auction is over. They may also choose to transform their bid for
      a new auction or to increase the bid by changing the value.
    
    -}
    (Bidding ptd mod) -> let !walletPkh  = ptPkh ptd
                             !walletAddr = createAddress walletPkh (ptSc ptd)
                             !txSigners  = V2.txInfoSignatories info
      in case redeemer of
        -- | Remove the UTxO from the contract.
        Remove -> traceIfFalse "Sign" (signedBy txSigners walletPkh)              -- seller must sign it
               && traceIfFalse "pays" (findPayout txOutputs walletAddr thisValue) -- seller must get the UTxO
               && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1)             -- single tx going in, no continue
        
        -- | Transform the auction bid
        Transform -> 
          case getOutboundDatum contTxOutputs of
            -- transform back into the bidding state
            (Bidding ptd' _) -> traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
                             && traceIfFalse "newo" (ptd == ptd')                   -- new owner must own it
                             && traceIfFalse "Ins"  (nInputs txInputs scriptAddr 1) -- single tx going in
                             && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out

            -- offer into a swappable
            (Swappable ptd' _ td') -> traceIfFalse "Sign" (signedBy txSigners walletPkh)  -- seller must sign it
                                   && traceIfFalse "ins"  (nInputs txInputs scriptAddr 1) -- single tx going in, no continue
                                   && traceIfFalse "Out"  (nOutputs contTxOutputs 1)      -- single going out
                                   && traceIfFalse "owns" (ptd == ptd')                   -- seller cant change
                                   && traceIfFalse "Auct" (checkValidTimeData td')        -- valid auction time lock

            -- other datums fail
            _ -> traceIfFalse "Bidding:Transform:Undefined Datum" False

        -- | Complete an auction with a specific auction UTxO.
        Complete ->  let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- swappable only
            (Auctioning ptd' atd _) -> let !sellerPkh           = ptPkh ptd'
                                           !sellerAddr          = createAddress sellerPkh (ptSc ptd')
                                           !auctionTimeInterval = lockBetweenTimeInterval (tStart atd) (tEnd atd)
                                           !txValidityRange     = V2.txInfoValidRange info
                                    in traceIfFalse "Sign" (signedBy txSigners sellerPkh)                            -- seller must sign it
                                    && traceIfFalse "pays" (findPayout txOutputs sellerAddr thisValue)               -- seller must get the UTxO
                                    && traceIfFalse "oldo" (ptd /= ptd')                                             -- cant sell this to self
                                    && traceIfFalse "ins"  (nInputs txInputs scriptAddr 2)                           -- double tx going in
                                    && traceIfFalse "Out"  (nOutputs contTxOutputs 1)                                -- single going out
                                    && traceIfFalse "Auct" (isTxOutsideInterval auctionTimeInterval txValidityRange) -- seller can unlock it
            
            -- anything else fails
            _ -> traceIfFalse "Bidding:Complete:Undefined Datum" False

        -- Other redeemers fail
        _ -> traceIfFalse "Bidding:Undefined Redeemer" False
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo  context

    txOutputs :: [V2.TxOut]
    txOutputs = V2.txInfoOutputs info

    txInputs :: [V2.TxInInfo]
    txInputs = V2.txInfoInputs info

    validatingInput :: V2.TxOut
    validatingInput = ownInput context

    thisValue :: V2.Value
    thisValue = V2.txOutValue validatingInput
    
    scriptAddr :: V2.Address
    scriptAddr = V2.txOutAddress validatingInput

    contTxOutputs :: [V2.TxOut]
    contTxOutputs = getScriptOutputs txOutputs scriptAddr

    createTxOutRef :: V2.BuiltinByteString -> Integer -> V2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: V2.TxOutRef
        txId = V2.TxOutRef
          { V2.txOutRefId  = V2.TxId { V2.getTxId = txHash }
          , V2.txOutRefIdx = index
          }
    
    checkServiceFeePayout :: CustomDatumType -> ReferenceDatum -> Bool
    checkServiceFeePayout d r = (findPayout txOutputs (cashAddr r) feeValue)
      where
        
        cashAddr :: ReferenceDatum -> V2.Address
        cashAddr (Reference ca _ _) = createAddress (caPkh ca) (caSc ca)
      
        feeValue :: V2.Value
        feeValue = Value.singleton Value.adaSymbol Value.adaToken (calculateServiceFee d r)

    getReferenceDatum :: V2.TxOut -> ReferenceDatum
    getReferenceDatum x = 
      case V2.txOutDatum x of
        V2.NoOutputDatum       -> traceError "No Datum"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum"
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @ReferenceDatum inline


    getOutboundDatumByValue :: [V2.TxOut] -> V2.Value -> CustomDatumType
    getOutboundDatumByValue txOuts val' = getOutboundDatumByValue' txOuts val'
      where
        getOutboundDatumByValue' :: [V2.TxOut] -> V2.Value -> CustomDatumType
        getOutboundDatumByValue' []     _   = traceError "Nothing Found By Value"
        getOutboundDatumByValue' (x:xs) val =
          if V2.txOutValue x == val -- strict value continue
            then
              case V2.txOutDatum x of
                V2.NoOutputDatum       -> getOutboundDatumByValue' xs val -- skip datumless
                (V2.OutputDatumHash _) -> traceError "Embedded Datum Found By Value"
                -- inline datum only
                (V2.OutputDatum (V2.Datum d)) -> 
                  case PlutusTx.fromBuiltinData d of
                    Nothing     -> traceError "Bad Data Found By Value"
                    Just inline -> PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
            else getOutboundDatumByValue' xs val
    
    getOutboundDatum :: [V2.TxOut] -> CustomDatumType
    getOutboundDatum txOuts = getOutboundDatum' txOuts
      where
        getOutboundDatum' :: [V2.TxOut] -> CustomDatumType
        getOutboundDatum' []     = traceError "Nothing Found On Cont"
        getOutboundDatum' (x:xs) =
          case V2.txOutDatum x of
            V2.NoOutputDatum       -> getOutboundDatum' xs
            (V2.OutputDatumHash _) -> traceError "Embedded Datum On Cont"
            -- inline datum only
            (V2.OutputDatum (V2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data On Cont"
                Just inline -> PlutusTx.unsafeFromBuiltinData @CustomDatumType inline

    -- this needs to not be able to reference the txId that is being spent
    getDatumByTxId :: V2.TxOutRef -> CustomDatumType
    getDatumByTxId txId = 
      case V2.txOutDatum $ V2.txInInfoResolved $ txInFromTxRef txInputs txId of
        V2.NoOutputDatum       -> traceError "No Datum On TxId"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum On TxId"
        -- inline datum only
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data On TxId"
            Just inline -> PlutusTx.unsafeFromBuiltinData @CustomDatumType inline

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

swapContractScriptShortBs :: SBS.ShortByteString
swapContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

swapContractScript :: PlutusScript PlutusScriptV2
swapContractScript = PlutusScriptSerialised swapContractScriptShortBs
