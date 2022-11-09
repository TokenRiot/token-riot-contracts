#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

swap_script_path="../../contracts/swap-contract/swap-contract.plutus"

# Addresses
reference_address=$(cat ../wallets/reference-wallet/payment.addr)

swap_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-reference-script-file ${swap_script_path} \
    --tx-out="${reference_address} 5000000" | tr -dc '0-9')

swap_script_reference_utxo="${reference_address} + ${swap_min_utxo}"

echo -e "Creating Reference Script:\n" ${swap_script_reference_utxo}
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
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${reference_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${swap_script_reference_utxo}" \
    --tx-out-reference-script-file ${swap_script_path} \
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
    --signing-key-file ../wallets/reference-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/swap-reference-utxo.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/swap-reference-utxo.signed