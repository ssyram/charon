import Data.Aeson (eitherDecodeFileStrict)
import Generated_GAst (TranslatedCrate(..))

main :: IO ()
main = do
  let filepath = "../charon/tests/ui/arrays.llbc"
  result <- eitherDecodeFileStrict filepath :: IO (Either String TranslatedCrate)
  case result of
    Left err -> putStrLn $ "Failed: " ++ err
    Right crate -> putStrLn $ "Success! Crate name: " ++ translatedCrateCrate_name crate
