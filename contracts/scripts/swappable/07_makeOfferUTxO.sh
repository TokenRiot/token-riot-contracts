#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staked smart contract address
script_path="../../swap-contract/swap-contract.plutus"
stake_path="../../swap-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# seller
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)

# buyer
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

# asset to trade
asset="1 53e4deacc6f8cd78f3490d64d199467df7a5893363d964b1cdba1a5d.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/offerable/buyer-offerable-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Offer OUTPUT: "${script_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${buyer_address} \
    --out-file ../tmp/buyer_utxo.json
TXNS=$(jq length ../tmp/buyer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${buyer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/buyer_utxo.json)
buyer_tx_in=${TXIN::-8}

# get script utxo data
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
TXIN=$(jq -r --arg alltxin "" --arg sellerPkh "${seller_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $sellerPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
sale_tx_in=${TXIN::-8}
string=${sale_tx_in}
IFS='#' read -ra array <<< "$string"
# update tx id info
variable=${array[0]}; jq --arg variable "$variable" '.fields[1].fields[0].bytes=$variable' ../data/offerable/buyer-offerable-datum.json > ../data/offerable/buyer-offerable-datum-new.json
mv ../data/offerable/buyer-offerable-datum-new.json ../data/offerable/buyer-offerable-datum.json
variable=${array[1]}; jq --argjson variable "$variable" '.fields[1].fields[1].int=$variable' ../data/offerable/buyer-offerable-datum.json > ../data/offerable/buyer-offerable-datum-new.json
mv ../data/offerable/buyer-offerable-datum-new.json ../data/offerable/buyer-offerable-datum.json

# update the min utxo with new datum
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/offerable/buyer-offerable-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_utxo} + ${asset}"

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/offerable/buyer-offerable-datum.json  \
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