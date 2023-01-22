#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)

# Addresses
sender_address=$(cat ../wallets/seller-wallet/payment.addr)
# receiver_address=$(cat ../wallets/seller-wallet/payment.addr)
receiver_address="addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp"

# Define Asset to be printed here
asset=""
return_asset="24000 0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

# asset to trade
asset="1 43f43dd9db42399642f170cc410c73dd0d4e094f4b317c3ce9279069.007e08f756ee74d9d925df9c9881c717cf3c5fd510aedec0bf992c7a5a4a397a + 1 43f43dd9db42399642f170cc410c73dd0d4e094f4b317c3ce9279069.0112b80ebdbaa0ed5fe4dfe57679d1474428fa9eee815ca58103b4d3da3912c3 + 1 43f43dd9db42399642f170cc410c73dd0d4e094f4b317c3ce9279069.0169f3226794a28a9cd8b3c7e9e98190a43249d93b09fb290197c045c2cad68b"

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



min_utxo=6000000

token_to_be_traded="${receiver_address} + 2500000000"
token_to_be_changed="${receiver_address} + ${min_utxo} + ${asset}"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
echo -e "Trading A Token:\n" ${token_to_be_changed}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${sender_address} \
    --out-file ../tmp/sender_utxo.json

TXNS=$(jq length ../tmp/sender_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${sender_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../tmp/sender_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${sender_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${token_to_be_changed}" \
    --testnet-magic ${testnet_magic})

    # --tx-out="${token_to_be_traded}" \
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