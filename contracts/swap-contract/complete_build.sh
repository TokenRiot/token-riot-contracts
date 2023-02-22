#!/bin/bash
set -e

# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

# build reference contract
echo -e "\033[1;35m Build Contracts \033[0m" 
cabal build -w ghc-8.10.7 -O2

echo -e "\033[1;35m Run Reference Contract \033[0m" 
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

echo -e "\033[1;35m Update Reference Datum \033[0m" 

caPkh=$(jq '.caPkh' start_info.json)
caSc=$(jq '.caSc' start_info.json)
servPerc=$(jq '.servicePerc' start_info.json)
servFee=$(jq '.serviceFee' start_info.json)
cancFee=$(jq '.cancellationFee' start_info.json)
pkh1=$(jq '.pkh1' start_info.json)
pkh2=$(jq '.pkh2' start_info.json)
pkh3=$(jq '.pkh3' start_info.json)
pkhs="[{\"bytes\": $pkh1}, {\"bytes\": $pkh2}, {\"bytes\": $pkh3}]"
thres=$(jq '.thres' start_info.json)
hotPkh=$(jq '.hotPkh' start_info.json)
poolId=$(jq '.poolId' start_info.json)
rewardPkh=$(jq '.rewardPkh' start_info.json)
rewardSc=$(jq '.rewardSc' start_info.json)
jq \
--argjson caPkh "$caPkh" \
--argjson caSc "$caSc" \
--argjson servPerc "$servPerc" \
--argjson servFee "$servFee" \
--argjson cancFee "$cancFee" \
--argjson pkhs "$pkhs" \
--argjson thres "$thres" \
--argjson hotPkh "$hotPkh" \
--argjson poolId "$poolId" \
--argjson rewardPkh "$rewardPkh" \
--argjson rewardSc "$rewardSc" \
'.fields[0].fields[0].bytes=$caPkh | 
.fields[0].fields[1].bytes=$caSc | 
.fields[1].fields[0].int=$servPerc | 
.fields[1].fields[1].int=$servFee | 
.fields[1].fields[2].int=$cancFee | 
.fields[2].fields[0].list |= ($pkhs | .[0:length]) | 
.fields[2].fields[1].int=$thres | 
.fields[2].fields[2].bytes=$hotPkh | 
.fields[3].fields[0].bytes=$poolId |
.fields[3].fields[1].bytes=$rewardPkh |
.fields[3].fields[2].bytes=$rewardSc
' \
../scripts/data/referencing/reference-datum.json | sponge ../scripts/data/referencing/reference-datum.json


echo -e "\033[1;35m Run Swap Contract \033[0m" 

# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .pid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .tkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn

python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./src/SwapContract.hs', './src/SwapContract-new.hs', $(cat start.pid))"
mv ./src/SwapContract-new.hs ./src/SwapContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./src/SwapContract.hs', './src/SwapContract-new.hs', $(cat start.tkn))"
mv ./src/SwapContract-new.hs ./src/SwapContract.hs

python3 -c "from update_contracts import changeLockHash;changeLockHash('./src/SwapContract.hs', './src/SwapContract-new.hs', $(cat ./reference.bytes))"
mv ./src/SwapContract-new.hs ./src/SwapContract.hs

cabal run swap-contract

# Get script address
cardano-cli address build --payment-script-file swap-contract.plutus --testnet-magic 1 --out-file validator.addr
echo -e "\nValidator Testnet Address:" $(cat validator.addr)

# Get plutus validator hash
cardano-cli transaction policyid --script-file swap-contract.plutus > validator.hash
echo -e "\nValidator Hash:" $(cat validator.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
echo -e "\nValidator Bytes:" $(cat validator.bytes)