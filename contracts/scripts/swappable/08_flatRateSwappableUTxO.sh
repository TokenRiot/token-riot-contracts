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

# buyer
buyer_address=$(cat ../wallets/buyer-wallet/payment.addr)
buyer_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/buyer-wallet/payment.vkey)

# collat
collat_address=$(cat ../wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/collat-wallet/payment.vkey)

#
selling_asset="1 29554843ec2823b1a3b1bf1abd21b1bb0862d5efa6dea0838c9da0ee.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730"

# asset to trade
selling_asset="9223372036854775807 53e4deacc6f8cd78f3490d64d199467df7a5893363d964b1cdba1a5d.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 e2af8590cdcb8f58fbf82a9f933b2186e1935a8f029ad2e4f1f72f40.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 3986794edd5007c55c30f9897313542cbb1d40b803f02870cf97bba4.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 3d9606a1ea8de95a987d6f7ab7b7b836fbdb677c0e389322a348b06e.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 5aea8a71bf7c3f09f84117b9932eb0d156289f84d685d8ccc90ff1e6.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 856d2912a6ab21eac38e70a2acc18c1328b91189f976c7035ccc9678.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 78e79454dcdaf41f01fd61ca0cddba7c4e3ab4f6ae1352d9e386c80f.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 be302b650175bca3e2baec337947cc1872260f348ebbedd8f2c24c28.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 a61252bdc62e955431410aa104774007454107d6fa3bcdb7842b93fb.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 1dca68270d036e04ca5c5f6b1b1d14671153a5443b9bc5899c74bcab.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 5cb840dd5094cc8219d01a997ba9656fd8020945d373c37f97b6a7b6.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 cd3e363d31a9c26aca43116b4edddbb278ba7e4bd2ca0fed1705bd70.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 1c55443748374308219539a6a2505a2cd9fb8b2f367cb3176650a29b.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 f6f114a9f11ff983ec0a51dca5fb6eb6e4fd3b03fb8fef103e825b77.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734
     + 9223372036854775807 c939f2c1e308b52114f030d2bd5d8fb8ca7082fd5352e2be0b3046f2.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

# # asset to trade
# selling_asset="1 4aa8f49a12e9eeaa45cf65c1db24e6bac563ed272145ec72b6c81fdb.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730 
#      + 1 52b0a53baf53c19cd0ccea078f4eb3e45537ac0498889ac146d2a215.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 5c2e705e8c75baca44902b59476211b8fa5b0ffe11de9d8b09b2f456.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 5f794d046b93ac60aec9ef5e6f2ce9bba022d7fedc9328b088839c16.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 66cdeca96ced54672d849bcde2b5679b064c7c5602b758e9116405ed.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 7088a885a273c2bbcb2629aa45bfaccf90516859da51c30c24949b98.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 78082f5621692a3dde0bc2b9b271e19e3e7750c791a5ff83702d8682.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 7ce548852b3b2fff45752615f47cb3e449c1665c5f197cb4d8c69dba.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 7f312461ab69c28468c199100aa7aecdad0f71f4155264f77891e51f.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 a193a418550603df4a2f88e8e8bc8b919c823e1d1b05eeace1b48033.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 ac0c4daa8485b83dce345b884c130fdfbaa9544cd8e3dec33ceaf881.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 bc33a67b964f47914a2e09b92332a6755d2e4a35445a25c657d2e0e1.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 c207ba811698592da25d7c2d0c41476baacce5dcf53f3084be116d68.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 c8e9790b2989b87ba8ef0a4b0fc38d2535bd616194af8b886f1f9422.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730
#      + 1 dc283b01a369c0b4ca7b19d23602230f5e5d15dd86a870a55e70bb8c.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6730"


# buying_asset=""

# payment_min_utxo=$(${cli} transaction calculate-min-required-utxo \
#     --babbage-era \
#     --protocol-params-file ../tmp/protocol.json \
#     --tx-out="${seller_address} + 5000000 + ${buying_asset}" | tr -dc '0-9')

seller_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/seller-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${selling_asset}" | tr -dc '0-9')

buyer_min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/buyer-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${selling_asset}" | tr -dc '0-9')

difference=$((${buyer_min_utxo} - ${seller_min_utxo}))

if [ "$difference" -lt "0" ]; then
    min_utxo=${seller_min_utxo}
    # update the increase ada in the redeemer
    variable=0; jq --argjson variable "$variable" '.fields[1].fields[0].int=$variable' ../data/redeemers/flatrate-redeemer.json > ../data/redeemers/flatrate-redeemer-new.json
    mv ../data/redeemers/flatrate-redeemer-new.json ../data/redeemers/flatrate-redeemer.json
else
    echo "Increase Min ADA by" ${difference}
    min_utxo=${buyer_min_utxo}
    # update the increase ada in the redeemer
    variable=${difference}; jq --argjson variable "$variable" '.fields[1].fields[0].int=$variable' ../data/redeemers/flatrate-redeemer.json > ../data/redeemers/flatrate-redeemer-new.json
    mv ../data/redeemers/flatrate-redeemer-new.json ../data/redeemers/flatrate-redeemer.json
fi

script_address_out="${script_address} + ${min_utxo} + ${selling_asset}"
# seller_address_out="${seller_address} + ${payment_min_utxo} + ${buying_asset}"
seller_address_out="${seller_address} + 123456789" # new flat payment
echo "Script OUTPUT: "${script_address_out}
echo "Payment OUTPUT: "${seller_address_out}
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
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' ../tmp/buyer_utxo.json)
collateral_tx_in=${CTXIN::-19}
buyer_tx_in=${TXIN::-8}

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

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --out-file ../tmp/tx.draft \
    --change-address ${buyer_address} \
    --tx-in ${buyer_tx_in} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/redeemers/flatrate-redeemer.json \
    --tx-out="${seller_address_out}" \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file ../data/swappable/buyer-swappable-datum.json \
    --required-signer-hash ${collat_pkh} \
    --required-signer-hash ${buyer_pkh} \
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