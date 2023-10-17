#!/usr/bin/bash
set -e

source .env

${cli} query tip ${network} | jq '.slot + 3600'
