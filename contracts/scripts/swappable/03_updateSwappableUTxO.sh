#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# staked smart contract address
script_path="../../swap-contract/swap-contract.plutus"
stake_path="../../swap-contract/stake-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --stake-script-file ${stake_path} --testnet-magic ${testnet_magic})

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

# seller
seller_address=$(cat ../wallets/seller-wallet/payment.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/seller-wallet/payment.vkey)

#
asset="1 29554843ec2823b1a3b1bf1abd21b1bb0862d5efa6dea0838c9da0ee.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730"

# asset to trade
asset="1 4aa8f49a12e9eeaa45cf65c1db24e6bac563ed272145ec72b6c81fdb.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730 
     + 1 52b0a53baf53c19cd0ccea078f4eb3e45537ac0498889ac146d2a215.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 5c2e705e8c75baca44902b59476211b8fa5b0ffe11de9d8b09b2f456.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 5f794d046b93ac60aec9ef5e6f2ce9bba022d7fedc9328b088839c16.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 66cdeca96ced54672d849bcde2b5679b064c7c5602b758e9116405ed.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 7088a885a273c2bbcb2629aa45bfaccf90516859da51c30c24949b98.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 78082f5621692a3dde0bc2b9b271e19e3e7750c791a5ff83702d8682.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 7ce548852b3b2fff45752615f47cb3e449c1665c5f197cb4d8c69dba.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 7f312461ab69c28468c199100aa7aecdad0f71f4155264f77891e51f.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 a193a418550603df4a2f88e8e8bc8b919c823e1d1b05eeace1b48033.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 ac0c4daa8485b83dce345b884c130fdfbaa9544cd8e3dec33ceaf881.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 bc33a67b964f47914a2e09b92332a6755d2e4a35445a25c657d2e0e1.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 c207ba811698592da25d7c2d0c41476baacce5dcf53f3084be116d68.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 c8e9790b2989b87ba8ef0a4b0fc38d2535bd616194af8b886f1f9422.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
     + 1 dc283b01a369c0b4ca7b19d23602230f5e5d15dd86a870a55e70bb8c.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730"

current_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/seller-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

updated_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/updated-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

difference=$((${updated_min_utxo} - ${current_min_utxo}))

if [ "$difference" -lt "0" ]; then
    min_utxo=${current_min_utxo}
    # update the increase ada in the redeemer
    variable=0; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ../data/redeemers/update-redeemer.json > ../data/redeemers/update-redeemer-new.json
    mv ../data/redeemers/update-redeemer-new.json ../data/redeemers/update-redeemer.json
else
    echo "Increase Min ADA by" ${difference}
    min_utxo=${updated_min_utxo}
    # update the increase ada in the redeemer
    variable=${difference}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' ../data/redeemers/update-redeemer.json > ../data/redeemers/update-redeemer-new.json
    mv ../data/redeemers/update-redeemer-new.json ../data/redeemers/update-redeemer.json
fi

script_address_out="${script_address} + ${min_utxo} + ${asset}"
echo "Update OUTPUT: "${script_address_out}
#
# exit
#
# seller utxos
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
seller_tx_in=${TXIN::-8}

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

# ref info
script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/swap-reference-utxo.signed )

# slot info
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 250))

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${seller_address} \
    --tx-in ${seller_tx_in} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/redeemers/update-redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/swappable/updated-swappable-datum.json  \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

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