#!/usr/bin/bash
set -e

source .node.env

#
script_path="contracts/reference-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})
#
ft_script_path="contracts/swap-contract.plutus"
ft_script_address=$(${cli} address build --payment-script-file ${ft_script_path} ${network})


#
seller_address=$(cat ${ROOT}/addresses/seller.addr)
buyer_address=$(cat ${ROOT}/addresses/buyer.addr)
attacker_address=$(cat ${ROOT}/addresses/attacker.addr)
reference_address=$(cat ${ROOT}/addresses/reference.addr)
collat_address=$(cat ${ROOT}/addresses/collat.addr)



echo
echo -e "\033[1;35m Reference Address:" 
echo -e "\n${script_address}\n";
${cli} query utxo --address ${script_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;35m Swap Address:" 
echo -e "\n${ft_script_address}\n";
${cli} query utxo --address ${ft_script_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;36m Seller Address:" 
echo -e "\n${seller_address}\n";
${cli} query utxo --address ${seller_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Buyer Address:" 
echo -e "\n${buyer_address}\n";
${cli} query utxo --address ${buyer_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Attacker Address:" 
echo -e "\n${attacker_address}\n";
${cli} query utxo --address ${attacker_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${reference_address}\n";
${cli} query utxo --address ${reference_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${collat_address}\n";
${cli} query utxo --address ${collat_address} ${network}
echo -e "\033[0m"