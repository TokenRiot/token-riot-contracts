#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=1

# minting policy
mint_path="policy/policy.script"

# collat, seller, reference
starter_address=$(cat ../../wallets/starter-wallet/payment.addr)
starter_pkh=$(${cli} address key-hash --payment-verification-key-file ../../wallets/starter-wallet/payment.vkey)
minter_pkh=$(${cli} address key-hash --payment-verification-key-file ../../wallets/minter-wallet/minter.vkey)

out_address="addr_test1qrvnxkaylr4upwxfxctpxpcumj0fl6fdujdc72j8sgpraa9l4gu9er4t0w7udjvt2pqngddn6q4h8h3uv38p8p9cq82qav4lmp"

# pid and tkn

policy_id=$(cardano-cli transaction policyid --script-file ${mint_path})
# token_name=$(echo -n "ThisIsOneStarterTokenForTesting9" | od -A n -t x1 | sed 's/ *//g' | tr -d '\n')
token_name="546f6b656e52696f742e696f"
# assets
mint_asset="1 ${policy_id}.${token_name}"

# mint utxo
utxo_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file ../../tmp/protocol.json \
    --tx-out="${out_address} + 2000000 + ${mint_asset}" | tr -dc '0-9')

starter_address_out="${starter_address} + ${utxo_value} + ${mint_asset}"
# starter_address_out="${out_address} + ${utxo_value} + ${mint_asset}"
echo "Mint OUTPUT: "${starter_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${starter_address} \
    --out-file ../../tmp/starter_utxo.json

TXNS=$(jq length ../../tmp/starter_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${starter_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ../../tmp/starter_utxo.json)
starter_tx_in=${TXIN::-8}

echo Starter UTxO: $starter_tx_in

# slot time info
slot=$(${cli} query tip --testnet-magic ${testnet_magic} | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 500))

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --out-file ../../tmp/tx.draft \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --change-address ${starter_address} \
    --tx-in ${starter_tx_in} \
    --tx-out="${starter_address_out}" \
    --required-signer-hash ${starter_pkh} \
    --mint-script-file ${mint_path} \
    --mint="${mint_asset}" \
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
    --signing-key-file ../../wallets/starter-wallet/payment.skey \
    --tx-body-file ../../tmp/tx.draft \
    --out-file ../../tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file ../../tmp/tx.signed
