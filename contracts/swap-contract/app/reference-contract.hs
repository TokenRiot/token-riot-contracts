import Prelude
import Cardano.Api
import ReferenceContract ( referenceContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "reference-contract.plutus" Nothing referenceContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
