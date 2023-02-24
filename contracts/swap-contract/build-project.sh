cabal build -w ghc-8.10.7 -O2
cabal run swap-contract

# Get script address
cardano-cli address build --payment-script-file swap-contract.plutus --testnet-magic 1097911063 --out-file validator.addr
echo -e "\nValidator Testnet Address:" $(cat validator.addr)

# Get plutus validator hash
cardano-cli transaction policyid --script-file swap-contract.plutus > validator.hash
echo -e "\nValidator Hash:" $(cat validator.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo -e "\nValidator Bytes:" $(cat validator.bytes)

cabal run reference-contract

# Get script address
cardano-cli address build --payment-script-file reference-contract.plutus --testnet-magic 1 --out-file reference.addr
echo -e "\nReference Testnet Address:" $(cat reference.addr)

# Get plutus validator hash
cardano-cli transaction policyid --script-file reference-contract.plutus > reference.hash
echo -e "\nReference Hash:" $(cat reference.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat reference.hash)';s=binascii.unhexlify(a);print([x for x in s])" > reference.bytes
echo -e "\nReference Bytes:" $(cat reference.bytes)


cabal run stake-contract

# create all the required files
cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic 1 --out-file stake.addr
echo -e "\nStake Testnet Address:" $(cat stake.addr)

cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
echo -e "\nStake Hash:" $(cat stake.hash)

python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
echo -e "\nStake Bytes:" $(cat stake.bytes)

cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus --out-file stake.cert
echo -e "\nStake Cert";cat stake.cert | jq

cardano-cli stake-address deregistration-certificate --stake-script-file stake-contract.plutus --out-file destake.cert

poolId=$(jq -r '.poolId' start_info.json)
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id ${poolId} --out-file deleg.cert
echo -e "\nDeleg Cert";cat deleg.cert | jq

cabal run cip68-contract

# Get script address
cardano-cli address build --payment-script-file swap-contract.plutus --testnet-magic 1097911063 --out-file cip68.addr
echo -e "\nCIP 68 Testnet Address:" $(cat cip68.addr)

# Get plutus cip68 hash
cardano-cli transaction policyid --script-file swap-contract.plutus > cip68.hash
echo -e "\nCIP 68 Hash:" $(cat cip68.hash)

# Get plutus cip68 byte representation
python3 -c "import binascii;a='$(cat cip68.hash)';s=binascii.unhexlify(a);print([x for x in s])" > cip68.bytes
echo -e "\nCIP 68 Bytes:" $(cat cip68.bytes)

# complete
echo "DONE"