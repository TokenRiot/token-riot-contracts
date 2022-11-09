#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)
#
SELLER_ADDRESS=$(cat ../wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat ../wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat ../wallets/reference-wallet/payment.addr)
collat_ADDRESS=$(cat ../wallets/collat-wallet/payment.addr)

#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo
echo -e "\033[1;36m Seller Address: \033[0m" 
echo -e "\n \033[1;36m ${SELLER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic ${testnet_magic}
#
