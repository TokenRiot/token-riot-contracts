#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# contract path
swap_script_path="../../swap-contract/swap-contract.plutus"
stake_script_path="../../swap-contract/stake-contract.plutus"
refer_script_path="../../swap-contract/reference-contract.plutus"
cip68_script_path="../../swap-contract/cip68-contract.plutus"
minter_script_path="../../swap-contract/minter-contract.plutus"

# Addresses
reference_address=$(cat ../wallets/reference-wallet/payment.addr)
# script_reference_address="addr_test1qq4m4tup6h0zv3xsgtz22yxmk86qrm9j7wt3aunhwpg7hjd5skxg5duh2s97g6pvq2wrqcpr76u8m8uhd4gh9uhzqnnq0eush3"
script_reference_address=$(cat ../wallets/reference-wallet/payment.addr)

swap_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${swap_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

swap_value=$((${swap_min_utxo}))
swap_script_reference_utxo="${script_reference_address} + ${swap_value}"

stake_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${stake_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

stake_value=$((${stake_min_utxo}))
stake_script_reference_utxo="${script_reference_address} + ${stake_value}"

ref_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${refer_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

ref_value=$((${ref_min_utxo}))
ref_script_reference_utxo="${script_reference_address} + ${ref_value}"

cip68_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${cip68_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

cip68_value=$((${cip68_min_utxo}))
cip68_script_reference_utxo="${script_reference_address} + ${cip68_value}"

minter_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${minter_script_path} \
    --tx-out="${script_reference_address} + 1000000" | tr -dc '0-9')

minter_value=$((${minter_min_utxo}))
minter_script_reference_utxo="${script_reference_address} + ${minter_value}"

echo -e "Creating Swap Script:\n" ${swap_script_reference_utxo}
echo -e "Creating Stake Script:\n" ${stake_script_reference_utxo}
echo -e "Creating Refer Script:\n" ${ref_script_reference_utxo}
echo -e "Creating CIP68 Script:\n" ${cip68_script_reference_utxo}
echo -e "Creating Minter Script:\n" ${minter_script_reference_utxo}
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

# swap
starting_reference_lovelace=$(jq '[.. | objects | .lovelace] | add' ../tmp/reference_utxo.json)

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${starting_reference_lovelace}" \
    --tx-out="${swap_script_reference_utxo}" \
    --tx-out-reference-script-file ${swap_script_path} \
    --fee 900000

FEE=$(cardano-cli transaction calculate-min-fee --tx-body-file ../tmp/tx.draft --testnet-magic ${testnet_magic} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
firstReturn=$((${starting_reference_lovelace} - ${swap_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in ${HEXTXIN} \
    --tx-out="${reference_address} + ${firstReturn}" \
    --tx-out="${swap_script_reference_utxo}" \
    --tx-out-reference-script-file ${swap_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-1.signed \
    --testnet-magic ${testnet_magic}

# staking
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
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft ${network} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

#
secondReturn=$((${firstReturn} - ${stake_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${stake_script_reference_utxo}" \
    --tx-out-reference-script-file ${stake_script_path} \
    --fee ${fee}
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-2.signed \
    --testnet-magic ${testnet_magic}

# reference
nextUTxO=$(${cli} transaction txid --tx-body-file ../tmp/tx.draft)
echo "Third in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${ref_script_reference_utxo}" \
    --tx-out-reference-script-file ${refer_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft ${network} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

thirdReturn=$((${secondReturn} - ${ref_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${thirdReturn}" \
    --tx-out="${ref_script_reference_utxo}" \
    --tx-out-reference-script-file ${refer_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-3.signed \
    --testnet-magic ${testnet_magic}

# cip 68
nextUTxO=$(${cli} transaction txid --tx-body-file ../tmp/tx.draft)
echo "Fourth in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${thirdReturn}" \
    --tx-out="${cip68_script_reference_utxo}" \
    --tx-out-reference-script-file ${cip68_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft ${network} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

fourthReturn=$((${thirdReturn} - ${cip68_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${fourthReturn}" \
    --tx-out="${cip68_script_reference_utxo}" \
    --tx-out-reference-script-file ${cip68_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-4.signed \
    --testnet-magic ${testnet_magic}

# minter
nextUTxO=$(${cli} transaction txid --tx-body-file ../tmp/tx.draft)
echo "fifth in the tx chain" $nextUTxO

echo -e "\033[0;36m Building Tx \033[0m"
${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${secondReturn}" \
    --tx-out="${minter_script_reference_utxo}" \
    --tx-out-reference-script-file ${minter_script_path} \
    --fee 900000

FEE=$(${cli} transaction calculate-min-fee --tx-body-file ../tmp/tx.draft ${network} --protocol-params-file ../tmp/protocol.json --tx-in-count 0 --tx-out-count 0 --witness-count 1)
echo $FEE
fee=$(echo $FEE | rev | cut -c 9- | rev)

fifthReturn=$((${fourthReturn} - ${minter_value} - ${fee}))

${cli} transaction build-raw \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --tx-in="${nextUTxO}#0" \
    --tx-out="${reference_address} + ${fifthReturn}" \
    --tx-out="${minter_script_reference_utxo}" \
    --tx-out-reference-script-file ${minter_script_path} \
    --fee ${fee}

echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/tx-5.signed \
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

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx-4.signed

${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/tx-5.signed

cp ../tmp/tx-1.signed ../tmp/swap-reference-utxo.signed
cp ../tmp/tx-2.signed ../tmp/stake-reference-utxo.signed 
cp ../tmp/tx-3.signed ../tmp/data-reference-utxo.signed 
cp ../tmp/tx-4.signed ../tmp/cip68-reference-utxo.signed 
cp ../tmp/tx-5.signed ../tmp/minter-reference-utxo.signed 

