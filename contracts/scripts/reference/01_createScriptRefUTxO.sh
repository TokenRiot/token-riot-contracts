#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# get current params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# staked smart contract address
script_path="../../swap-contract/script-ref-contract.plutus"
stake_path="../../stake-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# seller info
payee_address=$(cat ../wallets/profit-wallet/payment.addr)

# asset to trade
pid=$(cat ../../swap-contract/start_info.json | jq -r .starterPid)
tkn=$(cat ../../swap-contract/start_info.json | jq -r .starterTkn)
asset="1 ${pid}.${tkn}"

# add in the hashes
variable=$(cat ../../swap-contract/swap-validator.hash); jq --arg variable "$variable" '.fields[0].bytes=$variable' ../data/reference/script-reference-datum.json > ../data/reference/script-reference-datum-new.json
mv ../data/reference/script-reference-datum-new.json ../data/reference/script-reference-datum.json
variable=$(cat ../../swap-contract/offer-validator.hash); jq --arg variable "$variable" '.fields[1].bytes=$variable' ../data/reference/script-reference-datum.json > ../data/reference/script-reference-datum-new.json
mv ../data/reference/script-reference-datum-new.json ../data/reference/script-reference-datum.json
variable=$(cat ../../swap-contract/auction-validator.hash); jq --arg variable "$variable" '.fields[2].bytes=$variable' ../data/reference/script-reference-datum.json > ../data/reference/script-reference-datum-new.json
mv ../data/reference/script-reference-datum-new.json ../data/reference/script-reference-datum.json
variable=$(cat ../../swap-contract/bid-validator.hash); jq --arg variable "$variable" '.fields[3].bytes=$variable' ../data/reference/script-reference-datum.json > ../data/reference/script-reference-datum-new.json
mv ../data/reference/script-reference-datum-new.json ../data/reference/script-reference-datum.json

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/reference/script-reference-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Script OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${payee_address} \
    --out-file ../tmp/payee_utxo.json

# transaction variables
TXNS=$(jq length ../tmp/payee_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${payee_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/payee_utxo.json)
payee_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${payee_address} \
    --tx-in ${payee_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/reference/script-reference-datum.json \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/profit-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/script-reference-tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/script-reference-tx.signed