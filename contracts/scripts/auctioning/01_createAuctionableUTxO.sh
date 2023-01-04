#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

#
script_path="../../swap-contract/swap-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
#
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
#
asset="1 f61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248ea.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"

min_asset="5000000 + ${asset}"
# min_asset="5000000 + ${asset} + 18446744073709551615 d61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248ea.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6732"
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/auctionable/auctionable-datum.json \
    --tx-out="${script_address} ${min_asset}" | tr -dc '0-9')

min_utxo=2594620 # worst case for auction datum
sc_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Script OUTPUT: "${sc_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file ../tmp/seller_utxo.json

# transaction variables
TXNS=$(jq length ../tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${sc_address_out}" \
    --tx-out-inline-datum-file ../data/auctionable/auctionable-datum.json  \
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
    --signing-key-file ../wallets/seller-wallet/payment.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/swappable-tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/swappable-tx.signed