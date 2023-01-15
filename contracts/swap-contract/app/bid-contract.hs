import Prelude
import Cardano.Api
import BidContract ( bidContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "bid-contract.plutus" Nothing bidContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
