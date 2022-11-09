import           Prelude
import           Cardano.Api
import           SwapContract ( swapContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "swap-contract.plutus" Nothing swapContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
