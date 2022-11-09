#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat ../data/testnet.magic)


txid=$(${cli} transaction txid --tx-file ../tmp/tx.signed )"#1"

${cli} query utxo \
    --tx-in ${txid} \
    --testnet-magic ${testnet_magic} \
    --out-file ../tmp/time.check


endTime=$(cat ../tmp/time.check | jq -r '."'${txid}'"'.inlineDatum.fields[0].fields[6].int)

currentTime=$(echo `expr $(echo $(date +%s%3N)) + $(echo 0)`)

echo $(( $((${endTime} - ${currentTime})) / 1000 )) "Seconds"
echo $(( $((${endTime} - ${currentTime})) / (60*1000) )) "Minutes"