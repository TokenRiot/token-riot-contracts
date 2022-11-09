#!/bin/bash
set -e
# check if arg exists
if [[ $# -eq 0 ]] ; then
    echo 'Please Supply A Bid'
    exit 1
fi
# check if at least 1 ada
if [[ $(echo "${1} <= 1000000" | bc) == 1 ]]; then
    echo 'Bid Not Large Enough'
    exit 1
fi

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

#
script_path="../../contracts/swap-contract/swap-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${testnet_magic})
#

seller_pkh=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[0].bytes)
seller_sc=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[1].bytes)
if [ "$seller_sc" = "" ];
then
    seller_address=$(./bech32 addr_test <<< 60${seller_pkh})
else
    seller_address=$(./bech32 addr_test <<< 00${seller_pkh}${seller_sc})
fi
# bid info


buyer_address=$(cat ../wallets/seller-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)



#
asset="1 f61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248ea.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"
min_asset="5000000 + ${asset}"
min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/auctionable/auctionable-datum.json \
    --tx-out="${script_address} ${min_asset}" | tr -dc '0-9')
#
# echo $min_utxo
min_utxo=2594620 # worst case for auction datum

min_utxo=$(($min_utxo + ${1}))
sc_address_out="${script_address} + ${min_utxo} + ${asset}"


bidPkh=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[8].bytes)

# bid info
currentBid=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[4].int)
bidPid=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[2].bytes)
bidTkn=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[3].bytes)
# do correct bid address
if [ "$bidPkh" = "" ];
then
    echo $bidPkh
    echo "No Current Bidder"
    exit
else
    echo "Creating Bidder Address"
    bidSc=$(cat ../data/auctionable/auctionable-datum.json | jq -r .fields[0].fields[9].bytes)
    if [ "$bidSc" = "" ];
    then
        echo "No Current Bidder"
        bidAddr=$(./bech32 addr_test <<< 60${bidPkh})
    else
        echo $bidPkh $bidSc
        bidAddr=$(./bech32 addr_test <<< 00${bidPkh}${bidSc})
    fi
    # make output
    if [ "$bidPid" = "" ];
    then
        buyer_address_out="${bidAddr} + ${currentBid}"
    else
        buyer_address_out="${bidAddr} + ${min_utxo} + ${currentBid} ${bidPid}.${bidTkn}"
    fi
fi

variable=${buyer_pkh}; jq --arg variable "$variable" '.fields[0].fields[8].bytes=$variable' ../data/auctionable/auctionable-datum.json > ../data/auctionable/auctionable-datum-new.json
mv ../data/auctionable/auctionable-datum-new.json ../data/auctionable/auctionable-datum.json
variable=${1}; jq -r --argjson variable $variable '.fields[0].fields[4].int=$variable' ../data/auctionable/auctionable-datum.json > ../data/auctionable/auctionable-datum-new.json
mv ../data/auctionable/auctionable-datum-new.json ../data/auctionable/auctionable-datum.json


echo "Payback OUTPUT: "${buyer_address_out}
echo "Bid OUTPUT: "${sc_address_out}

#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
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
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' ../tmp/seller_utxo.json)
collateral_tx_in=${CTXIN::-19}
seller_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/script_utxo.json
# transaction variables
TXNS=$(jq length ../tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/swap-reference-utxo.signed )
# collat info
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)
collat_utxo="10e5b05d90199da3f7cb581f00926f5003e22aac8a3d5a33607cd4c57d13aaf3" # in collat wallet

slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 150))

if [ "$currentBid" = 0 ];
then
    echo -e "\033[0;36m Building First Bid \033[0m"
    FEE=$(${cli} transaction build \
        --babbage-era \
        --protocol-params-file ../tmp/protocol.json \
        --out-file ../tmp/tx.draft \
        --invalid-before ${current_slot} \
        --invalid-hereafter ${final_slot} \
        --change-address ${seller_address} \
        --tx-in ${seller_tx_in} \
        --tx-in-collateral="${collat_utxo}#0" \
        --tx-in ${script_tx_in}  \
        --spending-tx-in-reference="${script_ref_utxo}#1" \
        --spending-plutus-script-v2 \
        --spending-reference-tx-in-inline-datum-present \
        --spending-reference-tx-in-redeemer-file ../data/redeemers/bid-redeemer.json \
        --tx-out="${sc_address_out}" \
        --tx-out-inline-datum-file ../data/auctionable/auctionable-datum.json  \
        --required-signer-hash ${collat_pkh} \
        --required-signer-hash ${seller_pkh} \
        --required-signer-hash ${buyer_pkh} \
        --testnet-magic ${testnet_magic})
else
    echo ''
    echo -e "\033[0;36m Building Cont Bid \033[0m"
    FEE=$(${cli} transaction build \
        --babbage-era \
        --protocol-params-file ../tmp/protocol.json \
        --out-file ../tmp/tx.draft \
        --invalid-before ${current_slot} \
        --invalid-hereafter ${final_slot} \
        --change-address ${seller_address} \
        --tx-in ${seller_tx_in} \
        --tx-in-collateral="${collat_utxo}#0" \
        --tx-in ${script_tx_in}  \
        --spending-tx-in-reference="${script_ref_utxo}#1" \
        --spending-plutus-script-v2 \
        --spending-reference-tx-in-inline-datum-present \
        --spending-reference-tx-in-redeemer-file ../data/redeemers/bid-redeemer.json \
        --tx-out="${buyer_address_out}" \
        --tx-out="${sc_address_out}" \
        --tx-out-inline-datum-file ../data/auctionable/auctionable-datum.json  \
        --required-signer-hash ${collat_pkh} \
        --required-signer-hash ${seller_pkh} \
        --required-signer-hash ${buyer_pkh} \
        --testnet-magic ${testnet_magic})
fi

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ../wallets/seller-wallet/payment.skey \
    --signing-key-file ../wallets/buyer-wallet/payment.skey \
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