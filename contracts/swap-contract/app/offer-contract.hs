import Prelude
import Cardano.Api
import OfferContract ( offerContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "offer-contract.plutus" Nothing offerContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
