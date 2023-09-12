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

# seller info
seller="staked1"
seller_address=$(cat ../wallets/${seller}-wallet/base.addr)
seller_pkh=$(${cli} address key-hash --payment-verification-key-file ../wallets/${seller}-wallet/payment.vkey)

# asset to trade
asset="23523224 3228af28b011ccdf4f8293088894d988de8dcbc295a9e84c2fcd51e3.2834343429010713cc7f363e62a0c27ae9250d04c1ce5260c6ce891a77d1afd7 
+ 12351212 3d78e4d30a03db557222a66df75f9a9053ce7c44f9fb13487b2aacfa.001bc280005b86204c152f84ee3632ba2273a0b049b4f11da473ea92d3327d1f 
+ 1 43f43dd9db42399642f170cc410c73dd0d4e094f4b317c3ce9279069.0169f3226794a28a9cd8b3c7e9e98190a43249d93b09fb290197c045c2cad68b 
+ 1 4515744a5e564610479c541247676bdec242e957c7c4fee7c91dcf12.cc10b946769f2c939b7b59e6ba023dc8d0c53da2998436d98bc4b56f57f55b62 
+ 1345125323 6effa18e41008cd0b13f3959a5a4af40b92ca936bb7669f40d3b1f81.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6732 
+ 4234232 92158340b4f3147aea48dd4bdb961878ec56f51f4cf4786a4490cf4e.001bc280002212cbed97bfb70c162f3733a9c69af5ddbc20737fc6217518ad82 
+ 100000000 989b0b633446d55c994ce997634fd5f94bd4e530bfa041448ea75c9c.28343434290198e10f93b990f9eab9fd6d05b2d2a0a08c359f36f123a925c36d 
+ 2352342 bb822c7a59b0ca2d97319ef56ef52420e8e5dfd1992570291f315757.001bc28001d2a9edce8d52a95599c04664336800a5ce64cdf2ff24d991db2e95 
+ 13452363 c34332d539bb554707a2d8826f2057bc628ac433a779c2f43d4a5b5c.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731 
+ 1 e0fbf3c50833521e216506330f3a1c0805eda2559fadc3dbcd12a353.5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../tmp/protocol.json \
    --tx-out-inline-datum-file ../data/swappable/seller-swappable-datum.json \
    --tx-out="${script_address} + 5000000 + ${asset}" | tr -dc '0-9')

seller_address_out="${seller_address} + ${min_utxo} + ${asset}"
echo "Remove OUTPUT: "${seller_address_out}
#
# exit
#
# get seller utxo
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
# TXIN=$(jq -r --arg alltxin "" 'to_entries[] | select(.value.value | length < 2) | .key | . + $alltxin + " --tx-in"' ../tmp/seller_utxo.json)
seller_tx_in=${TXIN::-8}

echo Seller UTxO: $seller_tx_in


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
TXIN=$(jq -r --arg alltxin "" --arg sellerPkh "${seller_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $sellerPkh) | .key | . + $alltxin + " --tx-in"' ../tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

# for testing only
script_tx_in=$(cardano-cli transaction txid --tx-file ../tmp/swappable-tx.signed)#0

# script_tx_in="858c99423019e72f7878baec1550effc8e82dfb21b6aad89a30f2d18d02f3172#0"


# echo $script_tx_in
# exit

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
script_ref_utxo=$(${cli} transaction txid --tx-file ../tmp/swap-reference-utxo.signed )

# slot contraints
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 250))

    # --calculate-plutus-script-cost ../tmp/tx.cost \
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${seller_address} \
    --tx-in-collateral ${collat_tx_in} \
    --tx-in ${seller_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file ../data/redeemers/remove-redeemer.json \
    --tx-out="${seller_address_out}" \
    --required-signer-hash ${seller_pkh} \
    --required-signer-hash ${collat_pkh} \
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
    --signing-key-file ../wallets/${seller}-wallet/payment.skey \
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