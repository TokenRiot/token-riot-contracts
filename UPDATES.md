## v0.1.1

Finalizing the MVP scope with preview testing. Additional functionality is in the contract but MVP endpoints are heavily tested.

The contract now has two tiers of time locks because of the addition of auctions. The global lock in previous versions is top teir. The auction state is not in the MVP scope but early functionality is written. Advanced two wallet in-contract swaps are implemented but not tested that much.

Min utxos now are calculated using the worst case datum * the number of unique NFTs. This is to solve the min utxo problem introduced because of the datum. The min utxo required for an empty datum is not equal to the mix utxo for some utxo that has a price. This may be a little awkward but makes sense once you start using the contract. If users are planning on updating the datum a lot then this is a requirement else the user must remove and resend in their tokens to fix min utxos.

The 0.1.x releases are swap only as auctions are still in dev.

### tldr
- auction state implemention
    * bid, complete, close
- advance two wallet swaps
    * order book, utxo swap
- contract is to spec for MVP
    * e2e testing is complete for MVP scope
    * create, sale, update, remove

## v0.1.0

Adding swap functionality to the smart contract.

A utxo may enter the swap state with or without a time locked. The utxo may be updated later to change the locking information. A utxo that is time locked may not leave the contract. 

In the swap state there are many options for utxo interaction. A utxo may be sold for a flat rate of some primary token, an offer may be made to the original owner for the utxo, or two utxos can have their ownership swapped. All of these actions may occur in and out of a time lock.

The 0.1.x releases are swap only as auctions are still in dev.

### tldr
- swap state implementation.
    * flat rate sale, offer sale, utxo swap, and update utxo
- time lock is global
    * utxo can update into a lock or enter into a lock
- prep for auctions