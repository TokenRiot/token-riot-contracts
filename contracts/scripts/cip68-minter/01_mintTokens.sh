#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat ./path_to_socket.sh)
cli=$(cat ./path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file ../tmp/protocol.json

# staking contract
stake_script_path="../../swap-contract/stake-contract.plutus"

# cip 68 contract
cip68_script_path="../../swap-contract/cip68-contract.plutus"
cip68_script_address=$(${cli} address build --payment-script-file ${cip68_script_path} --stake-script-file ${stake_script_path} --testnet-magic ${testnet_magic})

#
hot_address=$(cat ../wallets/hot-wallet/hot.addr)
hot_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/hot-wallet/hot.vkey)

#
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)
#
receiver_address=$(cat ../wallets/seller-wallet/payment.addr)
receiver_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)

# the minting script policy
policy_id=$(cat ../../swap-contract/hashes/minter.hash)

echo -e "\033[0;36m Gathering NEWM UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${receiver_address} \
    --out-file ../tmp/receiver_utxo.json

TXNS=$(jq length ../tmp/receiver_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${receiver_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/receiver_utxo.json)
payee_tx_in=${TXIN::-8}

echo "Pay UTxO:" $payee_tx_in
first_utxo=$(jq -r 'keys[0]' ../tmp/receiver_utxo.json)
string=${first_utxo}
IFS='#' read -ra array <<< "$string"

prefix_100="28313030295265766f6c7574696f6e617279"
# prefix_333="2833333329"
# prefix_444="2834343429"
prefix_222="28323232295265766f6c7574696f6e617279"

# for testing
prefix_bad="283329"

ref_name=$(python3 -c "from getTokenName import token_name; token_name('${array[0]}', ${array[1]}, '${prefix_100}')")
nft_name=$(python3 -c "from getTokenName import token_name; token_name('${array[0]}', ${array[1]}, '${prefix_222}')")

echo -n $ref_name > ../tmp/reference.token
echo -n $frac_name > ../tmp/fraction.token

REFERENCE_ASSET="1 ${policy_id}.${ref_name}"
NFT_ASSET="1 ${policy_id}.${nft_name}"

MINT_ASSET="1 ${policy_id}.${ref_name} + 1 ${policy_id}.${nft_name}"

UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/cip68/metadata-datum.json \
    --tx-out="${cip68_script_address} + 5000000 + ${REFERENCE_ASSET}" | tr -dc '0-9')
reference_address_out="${cip68_script_address} + ${UTXO_VALUE} + ${REFERENCE_ASSET}"

UTXO_VALUE=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${receiver_address} + 5000000 + ${NFT_ASSET}" | tr -dc '0-9')
nft_address_out="${receiver_address} + ${UTXO_VALUE} + ${NFT_ASSET}"

echo "Reference Mint OUTPUT:" ${reference_address_out}
echo "NFT Mint OUTPUT:" ${nft_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file ../tmp/collat_utxo.json
TXNS=$(jq length ../tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/minter-reference-utxo.signed)
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

# Add metadata to this build function for nfts with data
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${hot_address} \
    --tx-in-collateral="${collat_utxo}" \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in ${payee_tx_in} \
    --tx-out="${reference_address_out}" \
    --tx-out-inline-datum-file ../data/cip68/metadata-datum.json \
    --tx-out="${nft_address_out}" \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${hot_pkh} \
    --mint="${MINT_ASSET}" \
    --mint-tx-in-reference="${script_ref_utxo}#1" \
    --mint-plutus-script-v2 \
    --policy-id="${policy_id}" \
    --mint-reference-tx-in-redeemer-file ../data/mint/mint-redeemer.json \
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
    --signing-key-file ../wallets/hot-wallet/hot.skey \
    --signing-key-file ../wallets/seller-wallet/payment.skey \
    --signing-key-file ../wallets/collat-wallet/payment.skey \
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

tx=$(cardano-cli transaction txid --tx-file ../tmp/tx.signed)
echo "Tx Hash:" $tx