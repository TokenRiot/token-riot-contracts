# Token Riot Marketplace

## Overview

The **Token Riot Marketplace** is a decentralized platform designed for the exchange of UTxOs (Unspent Transaction Outputs) between users. It provides an array of features tailored for decentralized trading, such as flat-rate sales, offers, auctions, and time locks. Furthermore, the marketplace boasts an advanced royalty payout system, facilitating fully customizable distinctive royalty payments.

## Features

- **Flat-Rate Sales**: Allows users to list tokens for sale at a predetermined price.
- **Offers**: Enables buyers to propose prices for listed items, with sellers having the option to accept or counter.
- **Auctions**: Provides a platform for users to set up and partake in auctions for a variety of assets.
- **Time Locks**: Supports transactions with preset schedules, facilitating deferred transfers.
- **Royalty Payouts**: Features an intricate royalty system that delivers distinctive payments to creators and content proprietors.
- **CIP68 Minter**: A dedicated contract designed for minting CIP68 NFTs (Non-Fungible Tokens).

## Smart Contract

The central functionality of the Token Riot Marketplace resides within a comprehensive smart contract available in this repository. This contract oversees all marketplace activities, guaranteeing secure and decentralized transactions.

### Testing

The marketplace can be compiled using the `complete_build.sh` script located in the `swap-contract` directory. This script relies on the details provided in the `start_info.json` file. The current configuration is set for the mainnet environment of Cardano.

For testing purposes, a happy path is available for the pre-production testnet environment, utilizing the `cardano-cli`. Interaction scripts for the smart contract can be found in the scripts directory. It's essential to have a set of test wallets, which can be generated using the `create_testnet_wallet.sh` script. The contract's functionalities are organized into distinct directories based on their actions: `swappable` for swapping UTxOs, `auctioning` for auction-related tasks, and `cip68-minter` for the CIP68 minter.

## License

This project operates under the GNU GPLv3 License.

## Contact

For questions, suggestions, or concerns, please reach out to us at help@tokenriot.io.