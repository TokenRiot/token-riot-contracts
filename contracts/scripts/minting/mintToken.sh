#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# minting policy
mint_path="policy/policy.script"

# collat, seller, reference
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)
minter_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/minter-wallet/minter.vkey)

out_address="addr_test1vrdhhl7yrfpufkwrzdpw8l29wsy6adqrq249ypvd7d6fzwc6x00av"

# pid and tkn
policy_id=$(cat policy/policy.id)
# token_name=$(echo -n "ThisIsOneStarterTokenForTesting9" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
token_name="5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"
# assets
mint_asset="1 ${policy_id}.${token_name}"

# mint utxo
utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${seller_address} + 2000000 + ${mint_asset}" | tr -dc '0-9')

seller_address_out="${seller_address} + ${utxo_value} + ${mint_asset}"
# seller_address_out="${out_address} + ${utxo_value} + ${mint_asset}"
echo "Mint OUTPUT: "${seller_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${seller_address} \
    --out-file ../tmp/seller_utxo.json

TXNS=$(jq length ../tmp/seller_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${seller_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-out="${seller_address_out}" \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${seller_pkh} \
    --mint-script-file policy/policy.script \
    --mint="${mint_asset}" \
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
    --signing-key-file ../wallets/minter-wallet/minter.skey \
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
