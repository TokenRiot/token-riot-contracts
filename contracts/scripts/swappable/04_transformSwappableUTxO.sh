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

# what was sold
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

return_asset="1 3930c07047eb432bef46975ed52086d9250e21c19ad5c0c05ac35fb1.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
        + 1 b98ceff806766d97d61a15deca05381c970d15d1d4da08869a3cdde8.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
        + 1 961bbeddb4dabf8432c0dae1988d633043e64d56d5181cdde36265a9.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
        + 1 08a1861472ae0b51049b21f8b58692bfb4bdec22d29169995bee74d6.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
        + 1 e0fbf3c50833521e216506330f3a1c0805eda2559fadc3dbcd12a353.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
        + 1 8df71632b3c9db50d19ec7a70457724188ef59be7a3b3bb0cabbba99.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
        + 1 06f25e8d1376360d8ad84ffc1add8c7808a62216b98866435eaf9d5e.0acb50dcbce90a495aaab9e6b47483eef1582556541b97e2b45484e6fe37fcf5
        + 1 43f43dd9db42399642f170cc410c73dd0d4e094f4b317c3ce9279069.007e08f756ee74d9d925df9c9881c717cf3c5fd510aedec0bf992c7a5a4a397a
        + 1 5f4db4def248cafd08fda9e33e84454370160b12f269c882504dd5a2.16eb4ff3b84147a5cc8e301d8488508b3748be4fe948d4a2c25a2106513458f8
        + 1 6effa18e41008cd0b13f3959a5a4af40b92ca936bb7669f40d3b1f81.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
        + 1 9e70589a8dd23fd55f65198dc9e9a135751690d6bb3a63f7c28389ef.3773f3d1e8485f82e8bdf4660a5865c76d2672c9edc804af502529794c5f3c9f
        + 1 ac74c28dcc6b051133f28ebb38cebfaf569f73a0b19ac8b9752c3796.012273743f3a872c685fcf6f7cd1ffa9157d8852b657b693178145aa83df13c3
        + 1 cffbbfaef33f2e8f9ce99b0b7610a6403d5cb03d44d5977be66f46cb.015592033a2850503565abef1188040c41a3429234c83f6cb41042db0a51b404
        + 1 d6784740a22d3bf55cbd99950035acaba1d7d71889f3f4d4866a9fa4.01c7e17dfef1086fd33930998b0d6e1fa4e8670362fc1e9369ff0580a3a64e7b
        + 1 df8e2ddd300b54a606611a8f20be1aef06dc38d560e00b73e57de5a1.01207256bd3ea524e30189ec2f76e3d6e99ab8e2e5252ac84c4eaa455df46d91"


# whats going back to the script
script_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/seller-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

# what is being returned to the seller
receiver_address="addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp"
return_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out="${receiver_address} + 5000000 + ${return_asset}" | tr -dc '0-9')



script_address_out="${script_address} + ${script_min_utxo} + ${asset}"
seller_address_out="${receiver_address} + ${return_min_utxo} + ${return_asset}"
echo "Transform OUTPUT: "${script_address_out}
echo "Return OUTPUT: "${seller_address_out}
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

# reference
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
    --spending-reference-tx-in-redeemer-file ../data/redeemers/transform-redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/swappable/seller-swappable-datum.json  \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

    # --tx-out-inline-datum-file ../data/auctionable/auctionable-datum.json  \
    # --tx-out-inline-datum-file ../data/offerable/seller-offerable-datum.json  \
    # --tx-out-inline-datum-file ../data/bidding/bidding-datum.json  \
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