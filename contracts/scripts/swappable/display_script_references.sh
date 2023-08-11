echo Swap: $(cardano-cli transaction txid --tx-file ../tmp/swap-reference-utxo.signed)#1
echo Stake $(cardano-cli transaction txid --tx-file ../tmp/stake-reference-utxo.signed)#1
echo Data: $(cardano-cli transaction txid --tx-file ../tmp/data-reference-utxo.signed)#1


# echo "CIP68"
# cardano-cli transaction txid --tx-file ../tmp/cip68-reference-utxo.signed 
# echo "MINT"
# cardano-cli transaction txid --tx-file ../tmp/minter-reference-utxo.signed 