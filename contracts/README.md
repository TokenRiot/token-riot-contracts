# A Generalized Swap Contract

A single contract to handle any and all types of token swapping.

## Set Up Guide

The first thing is setting up the swap data reference contract. This will create the script reference UTxOs and prep the data contract with the special starter token NFT.

```bash
cd scripts/referencing

./00_createScriptReferences.sh
./01_createReferenceUTxO.sh
```

