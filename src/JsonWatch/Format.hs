module JsonWatch.Format (pretty, prettyPrint) where


import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as AP
import           Data.String.Conversions  (convertString)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO


pretty :: A.Value -> T.Text
pretty = convertString . AP.encodePretty


prettyPrint :: A.Value -> IO ()
prettyPrint = TIO.putStrLn . pretty
