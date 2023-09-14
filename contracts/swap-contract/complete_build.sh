#!/bin/bash
set -e

network_flag="--testnet-magic 1"

# Complete Build
echo -e "\033[1;35m Starting... \033[0m"

mkdir -p addrs
mkdir -p hashes
mkdir -p bytes
mkdir -p certs

# build reference contract
echo -e "\033[1;35m Build Contracts \033[0m" 
cabal build -w ghc-8.10.7 -O2

###############################################################################
###############################################################################

echo -e "\033[1;35m Run Reference Contract \033[0m" 

rm addrs/reference.addr || true
rm hashes/reference.hash || true
rm bytes/reference.bytes || true

cabal run reference-contract

# Get script address
cardano-cli address build --payment-script-file reference-contract.plutus ${network_flag} --out-file addrs/reference.addr
echo -e "\nReference Testnet Address:" $(cat addrs/reference.addr)

# Get plutus validator hash
cardano-cli transaction policyid --script-file reference-contract.plutus > hashes/reference.hash
echo -e "\nReference Hash:" $(cat hashes/reference.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat hashes/reference.hash)';s=binascii.unhexlify(a);print([x for x in s])" > bytes/reference.bytes
echo -e "\nReference Bytes:" $(cat bytes/reference.bytes)

echo -e "\033[1;35m Update Reference Datum \033[0m" 

caPkh=$(jq '.caPkh' start_info.json)
caSc=$(jq '.caSc' start_info.json)
servPerc=$(jq '.servicePerc' start_info.json)
servFee=$(jq '.serviceFee' start_info.json)
cancFee=$(jq '.cancellationFee' start_info.json)
pkh1=$(jq '.pkh1' start_info.json)
pkh2=$(jq '.pkh2' start_info.json)
pkh3=$(jq '.pkh3' start_info.json)
# pkhs="[{\"bytes\": $pkh1}, {\"bytes\": $pkh2}, {\"bytes\": $pkh3}]"
pkhs="[{\"bytes\": $pkh1}]"
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
valid=$(python3 -c "import binascii;a='$(cat hashes/reference.hash)';s=binascii.unhexlify(a);print([x for x in s])")
jq \
--argjson pid "$pid" \
--argjson tkn "$tkn" \
--argjson valid "$valid" \
'.pid=$pid | 
.tkn=$tkn | 
.valid=$valid
' \
reference_info.json | sponge reference_info.json

###############################################################################
###############################################################################

echo -e "\033[1;35m Run Stake Contract \033[0m"

rm addrs/stake.addr || true
rm hashes/stake.hash || true
rm bytes/stake.bytes || true
rm certs/stake.cert || true
rm certs/destake.cert || true
rm certs/deleg.cert || true

cabal run stake-contract

# create all the required files
cardano-cli stake-address build --stake-script-file stake-contract.plutus ${network_flag} --out-file addrs/stake.addr
echo -e "\nStake Testnet Address:" $(cat addrs/stake.addr)

cardano-cli transaction policyid --script-file stake-contract.plutus > hashes/stake.hash
echo -e "\nStake Hash:" $(cat hashes/stake.hash)

python3 -c "import binascii;a='$(cat hashes/stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > bytes/stake.bytes
echo -e "\nStake Bytes:" $(cat bytes/stake.bytes)

cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus --out-file certs/stake.cert
echo -e "\nStake Cert";cat certs/stake.cert | jq

cardano-cli stake-address deregistration-certificate --stake-script-file stake-contract.plutus --out-file certs/destake.cert

poolId=$(jq -r '.poolId' start_info.json)
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id ${poolId} --out-file certs/deleg.cert
echo -e "\nDeleg Cert";cat certs/deleg.cert | jq

stakeHash=$(cat hashes/stake.hash)
jq \
--arg stakeHash "$stakeHash" \
'.fields[0].fields[0].bytes=$stakeHash' \
../scripts/data/staking/register-redeemer.json | sponge ../scripts/data/staking/register-redeemer.json

jq \
--arg stakeHash "$stakeHash" \
'.fields[0].fields[0].bytes=$stakeHash' \
../scripts/data/staking/withdraw-redeemer.json | sponge ../scripts/data/staking/withdraw-redeemer.json

###############################################################################
###############################################################################

echo -e "\033[1;35m Run Swap Contract \033[0m"

rm addrs/validator.addr || true
rm hashes/validator.hash || true
rm bytes/validator.bytes || true

cabal run swap-contract

# Get script address
cardano-cli address build --payment-script-file swap-contract.plutus --stake-script-file stake-contract.plutus ${network_flag} --out-file addrs/validator.addr
echo -e "\nValidator Testnet Address:" $(cat addrs/validator.addr)

# Get plutus validator hash
cardano-cli transaction policyid --script-file swap-contract.plutus > hashes/validator.hash
echo -e "\nValidator Hash:" $(cat hashes/validator.hash)

# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat hashes/validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > bytes/validator.bytes
echo -e "\nValidator Bytes:" $(cat bytes/validator.bytes)

###############################################################################
###############################################################################

echo -e "\033[1;35m Run CIP68 Contract \033[0m"

rm addrs/cip68.addr || true
rm hashes/cip68.hash || true
rm bytes/cip68.bytes || true

cabal run cip68-contract

# Get script address
cardano-cli address build --payment-script-file cip68-contract.plutus --stake-script-file stake-contract.plutus ${network_flag} --out-file addrs/cip68.addr
echo -e "\nCIP 68 Testnet Address:" $(cat addrs/cip68.addr)

# Get plutus cip68 hash
cardano-cli transaction policyid --script-file cip68-contract.plutus > hashes/cip68.hash
echo -e "\nCIP 68 Hash:" $(cat hashes/cip68.hash)

# Get plutus cip68 byte representation
python3 -c "import binascii;a='$(cat hashes/cip68.hash)';s=binascii.unhexlify(a);print([x for x in s])" > bytes/cip68.bytes
echo -e "\nCIP 68 Bytes:" $(cat bytes/cip68.bytes)

###############################################################################
###############################################################################

echo -e "\033[1;35m Run Minter Contract \033[0m"

rm hashes/minter.hash || true
rm bytes/minter.bytes || true

cabal run minter-contract

# Get plutus minter hash
cardano-cli transaction policyid --script-file minter-contract.plutus > hashes/minter.hash
echo -e "\nMinter Hash:" $(cat hashes/minter.hash)

# Get plutus minter byte representation
python3 -c "import binascii;a='$(cat hashes/minter.hash)';s=binascii.unhexlify(a);print([x for x in s])" > bytes/minter.bytes
echo -e "\nMinter Bytes:" $(cat bytes/minter.bytes)

###############################################################################
###############################################################################

# echo -e "\033[1;35m Updating TestSuite Contracts \033[0m"

# copy contracts into test-suite
# cp swap-contract.plutus reference-contract.plutus stake-contract.plutus cip68-contract.plutus minter-contract.plutus ../test-suite/contracts

# auto build the compiled code json
jq \
--arg reference_addr "$(cat addrs/reference.addr)" \
--arg swap_addr "$(cat addrs/validator.addr)" \
--arg cip68_addr "$(cat addrs/cip68.addr)" \
--arg stake_addr "$(cat addrs/stake.addr)" \
'.reference_addr=$reference_addr |
.swap_addr=$swap_addr |
.cip68_addr=$cip68_addr |
.stake_addr=$stake_addr
' \
compiled_code_info.json | sponge compiled_code_info.json
# complete

echo "# A Generalized Swap Contract" > ../README.md
echo "A set of contracts to handle any and all types of token swapping for Token Riot.\n" >> ../README.md
echo "\`\`\`json" >> ../README.md
cat compiled_code_info.json >> ../README.md
echo "\`\`\`" >> ../README.md

echo "DONE"