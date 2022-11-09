#!/bin/bash
set -e

echo -e "\033[1;35m Building Staking Contract... \033[0m" 
testnet_magic=$(cat ../../scripts/data/testnet.magic)

# get info
poolId=$(cat start_info.json | jq -r .poolId)
rewardPkh=$(cat start_info.json | jq -r .rewardPkh)
rewardSc=$(cat start_info.json | jq -r .rewardSc)

# stop build if pool or reward pkh are empty strings
if [ ! "${poolId}" ];then
   echo Empty Pool Id
   exit
fi

if [ ! "${rewardPkh}" ];then
   echo Empty Reward PKH
   exit
fi

echo 
echo -e "\033[1;33m Pool ID: ${poolId} \033[0m" 
echo 
echo -e "\033[1;34m Reward PKH ${rewardPkh} \033[0m" 
echo -e "\033[1;34m Reward Cred ${rewardSc} \033[0m" 
echo 

# starter nft data
python3 -c "import binascii;a='${poolId}';s=binascii.unhexlify(a);print([x for x in s])"    > pool.id
python3 -c "import binascii;a='${rewardPkh}';s=binascii.unhexlify(a);print([x for x in s])" > reward.pkh
python3 -c "import binascii;a='${rewardSc}';s=binascii.unhexlify(a);print([x for x in s])"  > reward.sc

# change the pool id
python3 -c "from update_contracts import changePoolId;changePoolId('./src/StakeContract.hs', './src/StakeContract.hs-new.hs', $(cat pool.id))"
mv ./src/StakeContract.hs-new.hs ./src/StakeContract.hs

# change payout pkh
python3 -c "from update_contracts import changeRewardPkh;changeRewardPkh('./src/StakeContract.hs', './src/StakeContract.hs-new.hs', $(cat reward.pkh))"
mv ./src/StakeContract.hs-new.hs ./src/StakeContract.hs

# change payout sc
python3 -c "from update_contracts import changeRewardSc;changeRewardSc('./src/StakeContract.hs', './src/StakeContract.hs-new.hs', $(cat reward.sc))"
mv ./src/StakeContract.hs-new.hs ./src/StakeContract.hs

# remove old data
rm stake.addr
rm stake.hash
rm stake.bytes
rm stake.cert
rm destake.cert
rm deleg.cert

# build out the stake-contract
cabal build -w ghc-8.10.7 -O2
cabal run stake-contract

# create all the required files
cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic ${testnet_magic} --out-file stake.addr
cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus --out-file stake.cert
cardano-cli stake-address deregistration-certificate --stake-script-file stake-contract.plutus --out-file destake.cert
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id ${poolId} --out-file deleg.cert

echo -e "\nStake Testnet Address:" $(cat stake.addr)
echo -e "\nStake Hash:" $(cat stake.hash)
echo -e "\nStake Bytes:" $(cat stake.bytes)
echo -e "\nStake Cert";cat stake.cert | jq
echo -e "\nDeleg Cert";cat deleg.cert | jq

# update the register redeemer to put the stake key on chain
variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../../scripts/data/redeemers/register-redeemer.json > ../../scripts/data/redeemers/register-redeemer-new.json
mv ../../scripts/data/redeemers/register-redeemer-new.json ../../scripts/data/redeemers/register-redeemer.json

variable=$(cat stake.hash); jq --arg variable "$variable" '.fields[0].fields[0].bytes=$variable' ../../scripts/data/redeemers/withdraw-redeemer.json > ../../scripts/data/redeemers/withdraw-redeemer-new.json
mv ../../scripts/data/redeemers/withdraw-redeemer-new.json ../../scripts/data/redeemers/withdraw-redeemer.json

echo -e "\nDONE\n"