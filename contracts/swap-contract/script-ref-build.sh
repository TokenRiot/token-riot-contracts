cabal build -w ghc-8.10.7 -O2
cabal run ref-contract

# Get plutus validator hash
cardano-cli transaction policyid --script-file script-ref-contract.plutus > script-reference.hash
echo -e "\nValidator Hash:" $(cat script-reference.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat script-reference.hash)';s=binascii.unhexlify(a);print([x for x in s])" > script-reference.bytes
echo -e "\nValidator Bytes:" $(cat script-reference.bytes)

# complete
echo "DONE"