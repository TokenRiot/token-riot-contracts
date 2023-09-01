#!/bin/bash
set -e

source .env

# get current params
${cli} query protocol-parameters ${network} --out-file ../tmp/protocol.json

# staked smart contract address
script_path="../../swap-contract/reference-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})

# seller info
starter_address=$(cat ../wallets/starter-wallet/payment.addr)

# asset to trade
pid=$(jq -r '.pid' ../../swap-contract/start_info.json)
tkn=$(jq -r '.tkn' ../../swap-contract/start_info.json)
asset="1 ${pid}.${tkn}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/referencing/reference-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Script OUTPUT: "${script_address_out}
#
exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
# get utxo
${cli} query utxo \
    ${network} \
    --address ${starter_address} \
    --out-file ../tmp/starter_utxo.json

# transaction variables
TXNS=$(jq length ../tmp/starter_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${starter_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/starter_utxo.json)
starter_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --change-address ${starter_address} \
    --tx-in ${starter_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/referencing/reference-datum.json \
    ${network})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/starter-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/referenceable-tx.signed \
    ${network}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file ../tmp/referenceable-tx.signed
