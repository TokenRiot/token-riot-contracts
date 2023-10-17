#!/bin/bash
set -e

# minting policy
mint_path="./minting/policy/policy.script"

# get pid
cardano-cli transaction policyid --script-file ${mint_path}