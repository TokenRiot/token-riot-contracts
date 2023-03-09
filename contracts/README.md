# A Generalized Swap Contract

A single contract to handle any and all types of token swapping.

## Set Up Guide

The first thing is setting up the swap data reference contract. This will create the script reference UTxOs and prep the data contract with the special starter token NFT.

```bash
cd scripts/referencing

./00_createScriptReferences.sh
./01_createReferenceUTxO.sh
```


The second thing is setting up the staking contract. The base address, swap + stake contract, needs to be register and delegated to the correct pool.

```bash
cd scripts/staking

./01_registerStake.sh
./02_delegateStake.sh
```

When rewards have accumlated use the `03_withdrawStakeRewards.sh` to withdraw the rewards to the reward address inside the data reference script.

### CIP 68 Metadata

If minting pfp and updatable metadata is required then we can use the CIP 68 contract with the hot key from the data reference contract.

- TODO

### Swap Contract