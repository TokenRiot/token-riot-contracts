import Prelude
import Cardano.Api
import ScriptRefContract ( refContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "script-ref-contract.plutus" Nothing refContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
