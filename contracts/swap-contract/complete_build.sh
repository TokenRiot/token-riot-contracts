#!/bin/bash
set -e

# Complete Build
echo -e "\033[1;35m Starting... \033[0m" 

# build reference contract
echo -e "\033[1;35m Build Contracts \033[0m" 
cabal build -w ghc-8.10.7 -O2

echo -e "\033[1;35m Run Reference Contract \033[0m" 

rm reference.addr
rm reference.hash
rm reference.bytes

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

pid=$(python3 -c "import binascii;a='$(jq -r '.pid' start_info.json)';s=binascii.unhexlify(a);print([x for x in s])")
tkn=$(python3 -c "import binascii;a='$(jq -r '.tkn' start_info.json)';s=binascii.unhexlify(a);print([x for x in s])")
valid=$(python3 -c "import binascii;a='$(cat reference.hash)';s=binascii.unhexlify(a);print([x for x in s])")
jq \
--argjson pid "$pid" \
--argjson tkn "$tkn" \
--argjson valid "$valid" \
'.pid=$pid | 
.tkn=$tkn | 
.valid=$valid
' \
reference_info.json | sponge reference_info.json

echo -e "\033[1;35m Run Swap Contract \033[0m"

rm validator.addr
rm validator.hash
rm validator.bytes

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

echo -e "\033[1;35m Run Stake Contract \033[0m"

rm stake.addr
rm stake.hash
rm stake.bytes
rm stake.cert
rm destake.cert
rm deleg.cert

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

stakeHash=$(cat stake.hash)
jq \
--arg stakeHash "$stakeHash" \
'.fields[0].fields[0].bytes=$stakeHash' \
../scripts/data/staking/register-redeemer.json | sponge ../scripts/data/staking/register-redeemer.json

jq \
--arg stakeHash "$stakeHash" \
'.fields[0].fields[0].bytes=$stakeHash' \
../scripts/data/staking/withdraw-redeemer.json | sponge ../scripts/data/staking/withdraw-redeemer.json

echo -e "\033[1;35m Run CIP68 Contract \033[0m"

rm cip68.addr
rm cip68.hash
rm cip68.bytes

cabal run cip68-contract

# Get script address
cardano-cli address build --payment-script-file cip68-contract.plutus --testnet-magic 1 --out-file cip68.addr
echo -e "\nCIP 68 Testnet Address:" $(cat cip68.addr)

# Get plutus cip68 hash
cardano-cli transaction policyid --script-file cip68-contract.plutus > cip68.hash
echo -e "\nCIP 68 Hash:" $(cat cip68.hash)

# Get plutus cip68 byte representation
python3 -c "import binascii;a='$(cat cip68.hash)';s=binascii.unhexlify(a);print([x for x in s])" > cip68.bytes
echo -e "\nCIP 68 Bytes:" $(cat cip68.bytes)

echo -e "\033[1;35m Updating TestSuite Contracts \033[0m"
# copy contracts into test-suite
cp swap-contract.plutus reference-contract.plutus stake-contract.plutus cip ../test-suite/contracts

# complete
echo "DONE"