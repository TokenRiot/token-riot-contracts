import Prelude
import Cardano.Api
import AuctionContract ( auctionContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "auction-contract.plutus" Nothing auctionContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
