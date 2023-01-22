#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staked smart contract address
script_path="../../swap-contract/swap-contract.plutus"
stake_path="../../stake-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# seller
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)

# buyer 
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

# asset to trade
asset="1 f48ed78c4c268158ef11ce050a96de452679e33a316acae2e9efdd7e.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 e184dcdf4df43652cfcb3c2b5e989d018f04cace48eaa9aa02811053.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 e20c67759540c2c4e5dfe7d7e93ed1b6692ebf2cfded8865f0e2d403.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 e3d98d8e62fe328468c0191759b3ad2ba5fbf1232b6b31070cdea848.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 f61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248ea.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731
     + 1 35a701244b5da5c238ad2cad27d750959f3ce0d0ff22766e73cc4efe.9cb3366523f23275f2618273f27d7f43d31dcab0890d3e65dc42a548a196a27a"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/bidding/bidding-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

#
script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Bid OUTPUT: "${script_address_out}
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
variable=${array[0]}; jq --arg variable "$variable" '.fields[1].fields[0].bytes=$variable' ../data/bidding/bidding-datum.json > ../data/bidding/bidding-datum-new.json
mv ../data/bidding/bidding-datum-new.json ../data/bidding/bidding-datum.json
variable=${array[1]}; jq --argjson variable "$variable" '.fields[1].fields[1].int=$variable' ../data/bidding/bidding-datum.json > ../data/bidding/bidding-datum-new.json
mv ../data/bidding/bidding-datum-new.json ../data/bidding/bidding-datum.json

# update the min utxo with new datum
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/bidding/bidding-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

script_address_out="${script_address} + ${min_utxo} + ${asset}"

echo -e "\033[0;36m Building Bid \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/bidding/bidding-datum.json  \
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