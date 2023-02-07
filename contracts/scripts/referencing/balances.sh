#!/usr/bin/bash
set -e
#
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

#
SCRIPT_PATH="../../swap-contract/reference-contract.plutus"
SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${SCRIPT_PATH} --testnet-magic ${testnet_magic})

#
SELLER_ADDRESS=$(cat ../wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat ../wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat ../wallets/reference-wallet/payment.addr)
DELEGATOR_ADDRESS=$(cat ../wallets/delegator-wallet/payment.addr)
COLLAT_ADDRESS=$(cat ../wallets/collat-wallet/payment.addr)

#
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json
${cli} query tip --testnet-magic ${testnet_magic} | jq

#
echo -e "\033[1;35m Script Address: \033[0m" 
echo -e "\n \033[1;35m ${SCRIPT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;36m Seller Address: \033[0m" 
echo -e "\n \033[1;36m ${SELLER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${SELLER_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;32m Buyer Address: \033[0m" 
echo -e "\n \033[1;32m ${BUYER_ADDRESS} \033[0m \n";
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;34m Delegator Address: \033[0m" 
echo -e "\n \033[1;34m ${DELEGATOR_ADDRESS} \033[0m \n";
${cli} query utxo --address ${DELEGATOR_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;34m Reference Address: \033[0m" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS} \033[0m \n";
${cli} query utxo --address ${REFERENCE_ADDRESS} --testnet-magic ${testnet_magic}

#
echo -e "\033[1;34m Collateral Address: \033[0m" 
echo -e "\n \033[1;34m ${COLLAT_ADDRESS} \033[0m \n";
${cli} query utxo --address ${COLLAT_ADDRESS} --testnet-magic ${testnet_magic}