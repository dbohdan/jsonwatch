-- Copyright (c) 2014, 2015, 2017 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

import qualified JsonWatch.Diff as JD

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

decodeText :: T.Text -> A.Value
decodeText t = fromMaybe (A.toJSON ()) $ A.decodeStrict $ encodeUtf8 t

decodeToFlatJsonList :: T.Text -> [(T.Text, T.Text)]
decodeToFlatJsonList t = HM.toList $ JD.flatten "" $ decodeText t

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Diff.flatten" $ do
        it "can decode simple values" $ do
            let values = ["5.0", "\"foo\"", "true", "false", "null"]
            let expected = [[("", T.dropAround (\x -> x == '"') x)] |
                            x <- values]
            [decodeToFlatJsonList x | x <- values] `shouldBe` expected

        it "can flatten nested values" $ do
            let v = "{\"surface\": {\"underground\": \
                \{\"deeper\": \"strange things\"}}}"
            let expected = [(".surface.underground.deeper", 
                             "strange things")]
            (decodeToFlatJsonList v) `shouldBe` expected

        it "formats numbers using standard decimal notation" $ do
            [decodeToFlatJsonList x | x <- ["1000000000", "0.00000001"]]
                `shouldBe` [[("", "1000000000.0")], [("", "0.00000001")]]

    describe "Diff.diff" $ do
        it "can diff nested values" $ do
            -- Based on data from
            -- http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua.
            v1 <- readFile "test/weather1.json"
            v2 <- readFile "test/weather2.json"
            let diff = JD.diff (JD.flatten "" $ decodeText $ T.pack v1)
                               (JD.flatten "" $ decodeText $ T.pack v2)
            diff `shouldBe` (HM.fromList
                [ (".weather.1.id", "520.0")
                , (".weather.1.description", "light intensity shower rain")
                , (".weather.1.main", "Rain"),
                (".weather.1.icon", "09d")],
                HM.fromList [(".name", ("Kiev", "Kyiv"))], HM.empty)

    describe "Diff.formatDiff" $ do
        it "can format Diffs" $ do
            let v1 = decodeText "{\"k1\": \"value\", \"k2\": {\"nested key\": \
                \[1, {\"deep\": \"space\"}]}, \"L\": 0}"
            let v2 = decodeText "{\"k1\": \"value\", \"k2\": {\"nested key\": \
                \[1, {\"deep\": \"sp@ce\"}]}, \"R\": 1}"
            let expectedDiff = [".k2['nested key'].1.deep: space -> sp@ce",
                                "- .L: 0.0",
                                "+ .R: 1.0"]
            JD.formatDiff (JD.diff (JD.flatten "" v1) (JD.flatten "" v2))
                `shouldBe` expectedDiff
