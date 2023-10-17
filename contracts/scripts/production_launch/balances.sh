#!/usr/bin/bash
set -e
#

source .env

#
SCRIPT_PATH="../../swap-contract/reference-contract.plutus"
SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${SCRIPT_PATH} ${network})

#
COLLAT_ADDRESS=$(cat ../wallets/collat-wallet/payment.addr)
STARTER_ADDRESS=$(cat ../wallets/starter-wallet/payment.addr)
KEEPER_ADDRESS=$(cat ../wallets/keeper-wallet/payment.addr)

#
${cli} query protocol-parameters ${network} --out-file ../tmp/protocol.json
${cli} query tip ${network} | jq

#
echo -e "\033[1;35m Script Address: \033[0m" 
echo -e "\n \033[1;35m ${SCRIPT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SCRIPT_ADDRESS} ${network}

#
echo -e "\033[1;34m Starter Address: \033[0m" 
echo -e "\n \033[1;34m ${STARTER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${STARTER_ADDRESS} ${network}

#
echo -e "\033[1;34m Keeper Address: \033[0m" 
echo -e "\n \033[1;34m ${KEEPER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${KEEPER_ADDRESS} ${network}

#
echo -e "\033[1;34m Collateral Address: \033[0m" 
echo -e "\n \033[1;34m ${COLLAT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${COLLAT_ADDRESS} ${network}
