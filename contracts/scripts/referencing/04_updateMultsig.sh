#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staked smart contract address
script_path="../../swap-contract/reference-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# deleg
deleg_address=$(cat ../wallets/delegator-wallet/payment.addr)
deleg_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/delegator-wallet/payment.vkey)

multisig1_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/multisig-wallet/multisig1.vkey)
multisig2_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/multisig-wallet/multisig2.vkey)

# asset to trade
asset="1 8df71632b3c9db50d19ec7a70457724188ef59be7a3b3bb0cabbba99.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

script_address_out="${script_address} + 5000000 + ${asset}"
echo "Update OUTPUT: "${script_address_out}
#
# exit
#
# get deleg utxo
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${deleg_address} \
    --out-file ../tmp/deleg_utxo.json

TXNS=$(jq length ../tmp/deleg_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${deleg_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/deleg_utxo.json)
deleg_tx_in=${TXIN::-8}

# get script utxo
echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/script_utxo.json
TXNS=$(jq length ../tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg delegPkh "${deleg_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $delegPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

# collat info
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
collat_tx_in=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

# script reference utxo
script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/data-reference-utxo.signed )

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${deleg_address} \
    --tx-in-collateral ${collat_tx_in} \
    --tx-in ${deleg_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/referencing/update-multisig-redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/referencing/reference-datum.json \
    --required-signer-hash ${deleg_pkh} \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${multisig1_pkh} \
    --required-signer-hash ${multisig2_pkh} \
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
    --signing-key-file ../wallets/delegator-wallet/payment.skey \
    --signing-key-file ../wallets/collat-wallet/payment.skey \
    --signing-key-file ../wallets/multisig-wallet/multisig1.skey \
    --signing-key-file ../wallets/multisig-wallet/multisig2.skey \
    --tx-body-file ../tmp/tx.draft \
    --out-file ../tmp/referenceable-tx.signed \
    --testnet-magic ${testnet_magic}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../tmp/referenceable-tx.signed

