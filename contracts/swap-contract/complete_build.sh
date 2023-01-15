# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .starterPid);s=binascii.unhexlify(a);print([x for x in s])" > starter.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .starterTkn);s=binascii.unhexlify(a);print([x for x in s])" > starter.tkn

echo -e "\033[1;36m Compile Script Reference Contract \033[0m"

cabal build -w ghc-8.10.7 -O2
cabal run ref-contract

# Get plutus validator hash
cardano-cli transaction policyid --script-file script-ref-contract.plutus > script-reference.hash
# Get plutus validator byte representation
python3 -c "import binascii;a='$(cat script-reference.hash)';s=binascii.unhexlify(a);print([x for x in s])" > script-reference.bytes

# nft minting validator hash
echo -e "\033[1;36m \nValidator Hash: $(cat script-reference.hash) \033[0m"
echo -e "\033[1;36m \nValidator Bytes: $(cat script-reference.bytes)\n \033[0m"

# offer
echo -e "\033[1;36m Compile Offer Contract \033[0m"

# adds in the reference hash to the offer contract
python3 -c "from update_contracts import changeRefHash;changeRefHash('./src/OfferContract.hs', './src/OfferContract-new.hs', $(cat script-reference.bytes))"
mv ./src/OfferContract-new.hs ./src/OfferContract.hs

# adds in the starter token to the offer contract
python3 -c "from update_contracts import changeStarterPid;changeStarterPid('./src/OfferContract.hs', './src/OfferContract-new.hs', $(cat starter.pid))"
mv ./src/OfferContract-new.hs ./src/OfferContract.hs
python3 -c "from update_contracts import changeStarterTkn;changeStarterTkn('./src/OfferContract.hs', './src/OfferContract-new.hs', $(cat starter.tkn))"
mv ./src/OfferContract-new.hs ./src/OfferContract.hs

cabal build -w ghc-8.10.7 -O2
cabal run offer-contract

# Get plutus validator hash
cardano-cli transaction policyid --script-file offer-contract.plutus > offer-validator.hash
echo -e "\033[1;36m \nValidator Hash: $(cat offer-validator.hash)\n \033[0m"


# bid
echo -e "\033[1;36m Compile Bid Contract \033[0m"

# adds in the reference hash to the bid contract
python3 -c "from update_contracts import changeRefHash;changeRefHash('./src/BidContract.hs', './src/BidContract-new.hs', $(cat script-reference.bytes))"
mv ./src/BidContract-new.hs ./src/BidContract.hs

# adds in the starter token to the bid contract
python3 -c "from update_contracts import changeStarterPid;changeStarterPid('./src/BidContract.hs', './src/BidContract-new.hs', $(cat starter.pid))"
mv ./src/BidContract-new.hs ./src/BidContract.hs
python3 -c "from update_contracts import changeStarterTkn;changeStarterTkn('./src/BidContract.hs', './src/BidContract-new.hs', $(cat starter.tkn))"
mv ./src/BidContract-new.hs ./src/BidContract.hs

cabal build -w ghc-8.10.7 -O2
cabal run bid-contract

# Get plutus validator hash
cardano-cli transaction policyid --script-file bid-contract.plutus > bid-validator.hash
echo -e "\033[1;36m \nValidator Hash: $(cat bid-validator.hash)\n \033[0m"

# swap
echo -e "\033[1;36m Compile Swap Contract \033[0m"

# adds in the reference hash to the swap contract
python3 -c "from update_contracts import changeRefHash;changeRefHash('./src/SwapContract.hs', './src/SwapContract-new.hs', $(cat script-reference.bytes))"
mv ./src/SwapContract-new.hs ./src/SwapContract.hs

# adds in the starter token to the swap contract
python3 -c "from update_contracts import changeStarterPid;changeStarterPid('./src/SwapContract.hs', './src/SwapContract-new.hs', $(cat starter.pid))"
mv ./src/SwapContract-new.hs ./src/SwapContract.hs
python3 -c "from update_contracts import changeStarterTkn;changeStarterTkn('./src/SwapContract.hs', './src/SwapContract-new.hs', $(cat starter.tkn))"
mv ./src/SwapContract-new.hs ./src/SwapContract.hs

cabal build -w ghc-8.10.7 -O2
cabal run swap-contract

# Get plutus validator hash
cardano-cli transaction policyid --script-file swap-contract.plutus > swap-validator.hash
echo -e "\033[1;36m \nValidator Hash: $(cat swap-validator.hash)\n \033[0m"

# auction
echo -e "\033[1;36m Compile Auction Contract \033[0m"

# adds in the reference hash to the auction contract
python3 -c "from update_contracts import changeRefHash;changeRefHash('./src/AuctionContract.hs', './src/AuctionContract-new.hs', $(cat script-reference.bytes))"
mv ./src/AuctionContract-new.hs ./src/AuctionContract.hs

# adds in the starter token to the auction contract
python3 -c "from update_contracts import changeStarterPid;changeStarterPid('./src/AuctionContract.hs', './src/AuctionContract-new.hs', $(cat starter.pid))"
mv ./src/AuctionContract-new.hs ./src/AuctionContract.hs
python3 -c "from update_contracts import changeStarterTkn;changeStarterTkn('./src/AuctionContract.hs', './src/AuctionContract-new.hs', $(cat starter.tkn))"
mv ./src/AuctionContract-new.hs ./src/AuctionContract.hs

cabal build -w ghc-8.10.7 -O2
cabal run auction-contract

# Get plutus validator hash
cardano-cli transaction policyid --script-file auction-contract.plutus > auction-validator.hash
echo -e "\033[1;36m \nValidator Hash: $(cat auction-validator.hash)\n \033[0m"