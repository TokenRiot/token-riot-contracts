cabal clean
cabal update
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
cardano-cli address build --payment-script-file reference-contract.plutus --testnet-magic 1097911063 --out-file reference.addr
echo -e "\nReference Testnet Address:" $(cat reference.addr)

# Get plutus validator hash
cardano-cli transaction policyid --script-file reference-contract.plutus > reference.hash
echo -e "\nReference Hash:" $(cat reference.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat reference.hash)';s=binascii.unhexlify(a);print([x for x in s])" > reference.bytes
echo -e "\nReference Bytes:" $(cat reference.bytes)

# complete
echo "DONE"