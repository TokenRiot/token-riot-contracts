#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staked smart contract address
script_path="../../swap-contract/swap-contract.plutus"
stake_path="../../swap-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

#
seller_address=$(cat ../wallets/seller-wallet/payment.addr)

# service fee
cash_register_address="addr_test1vqw9r7u88ud67gpyngp2gkuery77prlk60exs6cguga9cdqk3ygn2"

# buyer
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# asset to trade
selling_asset="20000000000 698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d.7444524950"

script_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/seller-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${selling_asset}" | tr -dc '0-9')

buyer_address_out="${buyer_address} + ${script_min_utxo} + ${selling_asset}"

payment_pid="$(jq -r '.fields[1].fields[0].bytes' ../data/swappable/seller-swappable-datum.json)"
payment_tkn="$(jq -r '.fields[1].fields[1].bytes' ../data/swappable/seller-swappable-datum.json)"
price=$(jq -r '.fields[1].fields[2].int' ../data/swappable/seller-swappable-datum.json)

if [ -z "$payment_pid" ]
then
    seller_address_out="${seller_address} + ${price}" # new flat payment

    feePerc=$(jq -r '.fields[1].fields[0].int' ../data/referencing/reference-datum.json)
    # 2.5% or 2 ADA for lovelace purchaces
    serviceFee=$(expr $price / $feePerc)
    if [ "$serviceFee" -lt "2000000" ]; then
        serviceFee=2000000
    fi
else
    payment_asset="${price} ${payment_pid}.${payment_tkn}"
    echo $payment_asset
    payment_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${seller_address} + 5000000 + ${payment_asset}" | tr -dc '0-9')

    seller_address_out="${seller_address} + ${payment_min_utxo} + ${payment_asset}" # new flat payment

    # non ada purchases default to the 2 ada
    serviceFee=2000000
fi


service_address_out="${cash_register_address} + ${serviceFee}"
royalty_address_out=$(python3 -c "from royaltyPayout import get_royalty_payout;a='../data/swappable/seller-swappable-datum.json';s=get_royalty_payout(a);print(s)")

#
echo "Buyer OUTPUT: "${buyer_address_out}
echo "Payment OUTPUT: "${seller_address_out}
echo "Service OUTPUT: "${service_address_out}
echo "Royalty OUTPUT: "${royalty_address_out}
#
# exit
#
# byuyer utxo info
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
echo Buyer TxIn: $buyer_tx_in

# script utxos
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
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}
echo Script TxIn: $script_tx_in

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
collat_utxo=$(jq -r 'keys[0]' ../tmp/collat_utxo.json)

# ref utxo
script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/swap-reference-utxo.signed )
data_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/referenceable-tx.signed )

# slot time info
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 150))

# This is just a silly way to do this imho
echo -e "\033[0;36m Building Tx \033[0m"
eval "FEE=\$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --read-only-tx-in-reference="${data_ref_utxo}#0" \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/redeemers/flatrate-remove-redeemer.json \
    "${royalty_address_out}" \
    --tx-out \"${seller_address_out}\" \
    --tx-out \"${buyer_address_out}\" \
    --tx-out \"${service_address_out}\" \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${buyer_pkh} \
    --testnet-magic ${testnet_magic})"

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