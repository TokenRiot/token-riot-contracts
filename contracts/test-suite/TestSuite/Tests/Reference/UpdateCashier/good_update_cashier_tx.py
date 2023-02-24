#!/usr/bin/python
"""
Test tokenization with a good transaction.
"""
import os
import copy
from dotenv import load_dotenv
from dotenv import load_dotenv, find_dotenv
from TestSuite.address import address_key_hash
import TestSuite.query as q
import TestSuite.parsing as p
import TestSuite.transaction as t

def good_update_cashier_tx():
    """
    Build a tokenization transaction that satisfies the validation logic and submits to the chain.
    """
    # env info
    root    = os.environ['ROOT']
    cli     = os.environ['cli']
    network = os.environ['network']

    tmp  = root+"/tmp/"
    addr = root+"/addresses/"
    
    # get the params
    q.protocol_parameters(cli, network, tmp)

    # get all the addrs and pkhs
    addrs = p.address_dict(addr)
    pkhs  = p.pkh_dict(cli, addr)
    sks   = p.skey_dict(addr)
    

    # ref utxos
    reference_ref = t.txid(cli, tmp+"tx-reference-utxo.signed") + "#1"
    # print(reference_ref)

    # get nft lock utxo data for the contract
    ref_contract_addr = addrs['reference-contract']
    q.utxo(cli, network, ref_contract_addr, tmp)
    script_tx_in, script_inline_datum, script_value = p.txin(tmp)
    # find out which one we are talking about here
    counter = 0
    for i in range(len(script_value)):
        if len(list(script_value[i].keys())) == 2:
            counter = i
    start = counter * 2
    if counter == 0:
        script_tx_in = script_tx_in[:2]
    else:
        script_tx_in = script_tx_in[start:start+2]

    # build out the current datum
    p.write_json_file(script_inline_datum[counter], 'data/referencing/reference-datum.json')

    # build out the next datum
    next_script_datum = copy.deepcopy(script_inline_datum[counter])
    p.write_json_file(next_script_datum, 'data/referencing/next-reference-datum.json')

    # create script output here

    ref_contract_output = p.process_output(ref_contract_addr, script_value[counter])
    # print('script output', ref_contract_output)

    # get the seller addr info
    seller_addr = addrs['seller']
    q.utxo(cli, network, seller_addr, tmp)
    seller_tx_in, seller_inline_datum, seller_value = p.txin(tmp)
    
    seller_output = p.process_output(seller_addr, seller_value[0])
    # print('seller output', seller_output)
    # quit()


    # get the collat addr info
    collat_addr = addrs['collat']
    q.utxo(cli, network, collat_addr, tmp)
    collat_tx_in, collat_inline_datum, collat_value = p.txin(tmp)

    # pkh for signing
    seller_pkh = pkhs['seller']
    collat_pkh = pkhs['collat']
    multisig1 = pkhs['multisig1']
    multisig2 = pkhs['multisig2']
    multisig3 = pkhs['multisig3']

    # build the output list

    utxo_out = [
        '--tx-out', seller_output,
        '--tx-out', ref_contract_output,
        '--tx-out-inline-datum-file', 'data/referencing/next-reference-datum.json',
    ]

    # build tx object for tx build function
    tx_object = {
        "change_addr": seller_addr,
        "collat_utxo": collat_tx_in[1],
        "utxo_in": seller_tx_in + script_tx_in,
        "spend_ref": reference_ref,
        "spend_redeemer": "data/referencing/update-cashier-redeemer.json",
        "utxo_out": utxo_out,
        "signers": [seller_pkh, collat_pkh, multisig1, multisig2, multisig3],
        "reference_utxos": []
    }

    outcome = t.build(cli, tmp, network, tx_object)
    # print(outcome)
    # quit()
    t.sign(cli, network, tmp, [sks['seller'], sks['collat'], sks['multisig1'], sks['multisig2'], sks['multisig3']])
    result = t.submit(cli, tmp, network)

    # update the current datum file
    p.write_json_file(next_script_datum, 'data/referencing/reference-datum.json')

    return result


if __name__ == "__main__":
    # Load environment variables from .node.env file
    load_dotenv(find_dotenv('.node.env'), verbose=False)

    # Set the CARDANO_NODE_SOCKET_PATH environment variable
    socket = os.environ['socket']
    os.environ["CARDANO_NODE_SOCKET_PATH"] = socket

    output = good_update_cashier_tx()
    print(output)
    
