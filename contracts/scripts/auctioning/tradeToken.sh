#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

# Addresses
sender_address=$(cat ../wallets/buyer-wallet/payment.addr)
receiver_address=$(cat ../wallets/seller-wallet/payment.addr)
# receiver_address="addr_test1qrxm0qpeek38dflguvrpp87hhewthd0mda44tnd45rjxqdt2s7gj5l4pam3pdeckkp7jwx8dsxelvq3ypv2ggzet9wcsxrp7pu"

seller_pkh=$(cardano-cli address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)
buyer_pkh=$(cardano-cli address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

# Define Asset to be printed here
# asset=0
asset="1 982f93a0efde8edd0e9af400da083e91d98e1d5b4a77a07938a4de4f.74686973697361766572796c6f6e67737472696e67666f7274657374696e3130"
return_asset=""

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${receiver_address} ${asset}" | tr -dc '0-9')
# token_to_be_traded="${receiver_address} + 333333333"
token_to_be_traded="${receiver_address} + ${min_utxo} + ${asset}"
token_to_be_changed="${sender_address} + ${min_utxo} + ${return_asset}"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${sender_address} \
    --out-file ../tmp/sender_utxo.json

TXNS=$(jq length ../tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${token_to_be_traded}" \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${buyer_pkh} \
    --testnet-magic ${testnet_magic})

    # --tx-out="${token_to_be_changed}" \
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
    --signing-key-file ../wallets/buyer-wallet/payment.skey \
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