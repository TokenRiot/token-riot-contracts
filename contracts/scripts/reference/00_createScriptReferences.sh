#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# contract path
stake_script_path="../../stake-contract/stake-contract.plutus"
ref_script_path="../../swap-contract/script-ref-contract.plutus"
swap_script_path="../../swap-contract/swap-contract.plutus"
bid_script_path="../../swap-contract/bid-contract.plutus"
offer_script_path="../../swap-contract/offer-contract.plutus"
auction_script_path="../../swap-contract/auction-contract.plutus"

# Addresses
reference_address=$(cat ../wallets/reference-wallet/payment.addr)

stake_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${stake_script_path} \
    --tx-out="${reference_address} + 1000000" | tr -dc '0-9')
stake_value=$((${stake_min_utxo}))
stake_script_reference_utxo="${reference_address} + ${stake_value}"

ref_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${ref_script_path} \
    --tx-out="${reference_address} + 1000000" | tr -dc '0-9')
ref_value=$((${stake_min_utxo}))
ref_script_reference_utxo="${reference_address} + ${ref_value}"

swap_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${swap_script_path} \
    --tx-out="${reference_address} + 1000000" | tr -dc '0-9')
swap_value=$((${swap_min_utxo}))
swap_script_reference_utxo="${reference_address} + ${swap_value}"

bid_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${bid_script_path} \
    --tx-out="${reference_address} + 1000000" | tr -dc '0-9')
bid_value=$((${bid_min_utxo}))
bid_script_reference_utxo="${reference_address} + ${bid_value}"

offer_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${offer_script_path} \
    --tx-out="${reference_address} + 1000000" | tr -dc '0-9')
offer_value=$((${offer_min_utxo}))
offer_script_reference_utxo="${reference_address} + ${offer_value}"

auction_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${auction_script_path} \
    --tx-out="${reference_address} + 1000000" | tr -dc '0-9')
auction_value=$((${auction_min_utxo}))
auction_script_reference_utxo="${reference_address} + ${auction_value}"


echo -e "Creating Stake Reference Script:\n" ${stake_script_reference_utxo}
echo -e "Creating Ref Reference Script:\n" ${ref_script_reference_utxo}
echo -e "Creating Swap Reference Script:\n" ${swap_script_reference_utxo}
echo -e "Creating Bid Reference Script:\n" ${bid_script_reference_utxo}
echo -e "Creating Offer Reference Script:\n" ${offer_script_reference_utxo}
echo -e "Creating Auction Reference Script:\n" ${auction_script_reference_utxo}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${reference_address} \
    --out-file ../tmp/reference_utxo.json

TXNS=$(jq length ../tmp/reference_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${reference_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/reference_utxo.json)
HEXTXIN=${TXIN::-8}
#
# exit
#
# chain second set of reference scripts to the first
echo -e "\033[0;36m Building Tx \033[0m"

starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' ../tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${swap_script_reference_utxo}" \
    --tx-out-reference-script-file ${swap_script_path} \
    --tx-out="${offer_script_reference_utxo}" \
    --tx-out-reference-script-file ${offer_script_path} \
    --fee 900000
FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
firstReturn=$((${starting_reference_lovelace} - ${swap_value} - ${offer_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${swap_script_reference_utxo}" \
    --tx-out-reference-script-file ${swap_script_path} \
    --tx-out="${offer_script_reference_utxo}" \
    --tx-out-reference-script-file ${offer_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}

nextUTxO=$(${cli} transaction txid --tx-body-file ../tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --tx-out="${ref_script_reference_utxo}" \
    --tx-out-reference-script-file ${ref_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft ${network} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
secondReturn=$((${firstReturn} - ${stake_value} - ${ref_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --tx-out="${ref_script_reference_utxo}" \
    --tx-out-reference-script-file ${ref_script_path} \
    --fee ${fee}
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-2.signed \
    --testnet-magic ${testnet_magic}


nextUTxO=$(${cli} transaction txid --tx-body-file ../tmp/tx.draft)
echo "First in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${bid_script_reference_utxo}" \
    --tx-out-reference-script-file ${bid_script_path} \
    --tx-out="${auction_script_reference_utxo}" \
    --tx-out-reference-script-file ${auction_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft ${network} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

thirdReturn=$((${secondReturn} - ${auction_value} - ${bid_value} - ${fee}))

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${thirdReturn}" \
    --tx-out="${bid_script_reference_utxo}" \
    --tx-out-reference-script-file ${bid_script_path} \
    --tx-out="${auction_script_reference_utxo}" \
    --tx-out-reference-script-file ${auction_script_path} \
    --fee ${fee}
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-3.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx-1.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx-2.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx-3.signed
#
cp ../tmp/tx-1.signed ../tmp/swap-reference-utxo.signed
cp ../tmp/tx-2.signed ../tmp/stake-reference-utxo.signed 
cp ../tmp/tx-3.signed ../tmp/auction-reference-utxo.signed 
