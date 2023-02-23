#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staked smart contract address
script_path="../../swap-contract/swap-contract.plutus"
stake_path="../../swap-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# service fee
deleg_address=$(cat ../wallets/delegator-wallet/payment.addr)

# seller
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)

# buyer
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

# asset to trade
auction_asset="1 5cb840dd5094cc8219d01a997ba9656fd8020945d373c37f97b6a7b6.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

# asset to trade
bid_asset="1 53e4deacc6f8cd78f3490d64d199467df7a5893363d964b1cdba1a5d.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"


bid_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/bidding/bidding-datum.json \
    --tx-out="${script_address} + 5000000 + ${bid_asset}" | tr -dc '0-9')

current_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/auctionable/seller-auctionable-datum.json \
    --tx-out="${script_address} + 5000000 + ${auction_asset}" | tr -dc '0-9')

next_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/buyer-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${auction_asset}" | tr -dc '0-9')

difference=$((${next_min_utxo} - ${current_min_utxo}))

if [ "$difference" -lt "0" ]; then
    min_utxo=${current_min_utxo}
    # update the increase ada in the redeemer
    variable=0; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ../data/redeemers/offer-redeemer.json > ../data/redeemers/offer-redeemer-new.json
    mv ../data/redeemers/offer-redeemer-new.json ../data/redeemers/offer-redeemer.json
else
    echo "Increase Min ADA by" ${difference}
    min_utxo=${next_min_utxo}
    # update the increase ada in the redeemer
    variable=${difference}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ../data/redeemers/offer-redeemer.json > ../data/redeemers/offer-redeemer-new.json
    mv ../data/redeemers/offer-redeemer-new.json ../data/redeemers/offer-redeemer.json
fi

script_address_out="${script_address} + ${min_utxo} + ${auction_asset}"
seller_address_out="${seller_address} + ${bid_min_utxo} + ${bid_asset}"
service_address_out="${deleg_address} + 2000000"
echo "Script OUTPUT: "${script_address_out}
echo "Payment OUTPUT: "${seller_address_out}
echo "Service OUTPUT: "${service_address_out}

#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file ../tmp/seller_utxo.json
TXNS=$(jq length ../tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}


echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/script_utxo.json
# transaction variables
TXNS=$(jq length ../tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg sellerPkh "${seller_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $sellerPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
auction_tx_in=${TXIN::-8}

TXIN=$(jq -r --arg alltxin "" --arg buyerPkh "${buyer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $buyerPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
bid_tx_in=${TXIN::-8}

string=${bid_tx_in}
IFS='#' read -ra array <<< "$string"
# update tx id info
variable=${array[0]}; jq --arg variable "$variable" '.fields[1].fields[0].bytes=$variable' ../data/redeemers/offer-redeemer.json > ../data/redeemers/offer-redeemer-new.json
mv ../data/redeemers/offer-redeemer-new.json ../data/redeemers/offer-redeemer.json
variable=${array[1]}; jq --argjson variable "$variable" '.fields[1].fields[1].int=$variable' ../data/redeemers/offer-redeemer.json > ../data/redeemers/offer-redeemer-new.json
mv ../data/redeemers/offer-redeemer-new.json ../data/redeemers/offer-redeemer.json

# collat info
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file ../tmp/collat_utxo.json
TXNS=$(jq length ../tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

# script reference
script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/swap-reference-utxo.signed )
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

# time info
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 150))

    # --calculate-plutus-script-cost ../tmp/tx.cost \
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${auction_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/redeemers/offer-redeemer.json \
    --tx-in ${bid_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/redeemers/complete-redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${service_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/swappable/buyer-swappable-datum.json \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${seller_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE

# echo TOTAL MEMORY
# jq -r  '[.[].executionUnits.memory] | add' ../tmp/tx.cost
#
exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/seller-wallet/payment.skey \
    --signing-key-file ../wallets/collat-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx.signed