import json

import subprocess
import json

def to_addr(base):
    func = [
        './bech32',
        'addr_test',
    ]
    p = subprocess.Popen(func, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    output = p.communicate(input=base.encode())[0].decode('utf-8').rstrip()
    return output


def get_data(file_path):
    # Load the JSON data from a file
    with open(file_path) as f:
        data = json.load(f)
    return data

def get_royalty_payout(file_path):
    data = get_data(file_path)

    addr_info = data['fields'][3]['fields'][0]['list']
    amt_info = data['fields'][3]['fields'][1]['list']
    if len(addr_info) == 0 and len(amt_info) == 0:
        return ""
    royalty_out = '--tx-out \"'
    for a,b in zip(addr_info, amt_info):
        pkh = a['fields'][0]['bytes']
        sc = a['fields'][1]['bytes']
        amt = b['int']
        if sc == "":
            addr = to_addr("60"+pkh)
        else:
            addr = to_addr("00"+pkh+sc)
        output = addr + " + " + str(amt)
        royalty_out += output + '\"     --tx-out \"'
    return royalty_out[:-15]

if __name__ == "__main__":
    data = get_royalty_payout('../data/swappable/seller-swappable-datum.json')
    print(data)
