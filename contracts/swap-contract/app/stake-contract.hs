{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
import           Prelude
import           Cardano.Api
import           Data.Aeson
import           GHC.Generics
import           System.Directory
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Data.ByteString.Lazy as BS
import qualified UsefulFuncs          ( createBuiltinByteString )
import           Data.ByteString.Lazy ( ByteString )
import           StakeContract        ( stakingPlutusScript, ScriptParameters(..) )

-- Define a type for the JSON data
data MyData = MyData
  { pid   :: [Integer]
  , tkn   :: [Integer]
  , valid :: [Integer]
  } deriving (Show, Generic)

instance FromJSON MyData

-- Read the JSON file into a ByteString
readJsonFile :: FilePath -> IO ByteString
readJsonFile filePath = BS.readFile filePath

-- Parse the ByteString into a MyData value
parseJson :: ByteString -> Maybe MyData
parseJson = decode

main :: IO ()
main = do
  let filePath = "reference_info.json"
  cwd <- getCurrentDirectory
  putStrLn $ "Current working directory: " ++ cwd
  putStrLn $ "Reading file: " ++ filePath
  jsonData <- readJsonFile filePath
  case parseJson jsonData of
    Just myData ->
      let sPid      = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = UsefulFuncs.createBuiltinByteString $ pid myData }
          sTkn      = PlutusV2.TokenName { PlutusV2.unTokenName = UsefulFuncs.createBuiltinByteString $ tkn myData }
          sValid    = PlutusV2.ValidatorHash $ UsefulFuncs.createBuiltinByteString $ valid myData
          sp        = ScriptParameters { lockPid = sPid
                                       , lockTkn = sTkn
                                       , refHash = sValid
                                       }
      in do
        result <- writeFileTextEnvelope "stake-contract.plutus" Nothing (stakingPlutusScript sp) 
        case result of
          Left err -> print $ displayError err
          Right () -> return ()
    Nothing -> putStrLn "Failed to parse JSON data"