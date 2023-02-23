#!/usr/bin/bash
set -e

source .node.env

echo -e "\033[1;35m Creating Catalog Starter Token \033[0m"
echo

reference_script_path="contracts/reference-contract.plutus"
spo_addr=$(cat ${ROOT}/addresses/payment3.addr)
echo spo $spo_addr
script_address=$(${cli} address build --payment-script-file ${reference_script_path} ${network})
echo script $script_address

cardano-cli query utxo --address ${spo_addr} ${network} --out-file ${ROOT}/tmp/spo_utxo.json

TXNS=$(jq length ${ROOT}/tmp/spo_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${spo_addr}! \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' ${ROOT}/tmp/spo_utxo.json)
spo_tx_in=${TXIN::-8}
echo "SPO TxIn: $spo_tx_in"

# Get multisig
minter_pkh=$(${cli} address key-hash --payment-verification-key-file ${ROOT}/addresses/minter.vkey)

policy_id=$(cat policy/policy.id)
token_name="5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"
mint_asset="1 ${policy_id}.${token_name}"

echo "Starter Token: ${mint_asset}"

script_address_out="${script_address} + 5000000 + ${mint_asset}"

script_address_out="${script_address} + 5000000 + ${mint_asset}"
bad_script_address_out="${script_address} + 5000000"

# exit
echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file ${ROOT}/tmp/protocol-parameters.json \
    --out-file ${ROOT}/tmp/tx.draft \
    --change-address ${spo_addr} \
    --tx-in ${spo_tx_in} \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/referencing/reference-datum.json  \
    --tx-out="${bad_script_address_out}" \
    --tx-out-inline-datum-file data/referencing/reference-datum.json  \
    --mint-script-file policy/policy.script \
    --mint="${mint_asset}" \
    --required-signer-hash ${minter_pkh} \
    ${network})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
#exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file ${ROOT}/stake-delegator-keys/payment3.skey \
    --signing-key-file ${ROOT}/addresses/minter.skey \
    --tx-body-file ${ROOT}/tmp/tx.draft \
    --out-file ${ROOT}/tmp/tx.signed \
    ${network}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    ${network} \
    --tx-file ${ROOT}/tmp/tx.signed