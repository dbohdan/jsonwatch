-- Copyright (c) 2014, 2015, 2017, 2018, 2019 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

import qualified JsonWatch.Diff     as JD
import qualified JsonWatch.Path     as JP

import qualified Data.Aeson         as A
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Test.Hspec

decodeText :: T.Text -> A.Value
decodeText t = fromMaybe (A.toJSON ()) $ A.decodeStrict $ encodeUtf8 t

decodeToFlatJsonList :: T.Text -> [(JP.Path, T.Text)]
decodeToFlatJsonList t = M.toAscList $ JD.flatten JP.empty $ decodeText t

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Diff.flatten" $ do
        it "can decode simple values" $ do
            let values = ["5", "\"foo\"", "true", "false", "null"]
            let expected =
                    [ [(JP.empty,
                        T.dropAround (\x -> x == '"') x)] | x <- values ]
            [ decodeToFlatJsonList x | x <- values ] `shouldBe` expected

        it "can flatten nested values" $ do
            let
                v
                    = "{\"surface\": {\"underground\": \
                \{\"deeper\": \"strange things\"}}}"
            let expected = [(JP.addText "deeper" $ JP.addText "underground" $
                             JP.addText "surface" $ JP.empty,
                             "strange things")]
            (decodeToFlatJsonList v) `shouldBe` expected

        it "formats numbers using standard decimal notation" $ do
            [ decodeToFlatJsonList x | x <- ["1000000000", "0.00000001"] ]
                `shouldBe` [[(JP.empty, "1000000000")],
                            [(JP.empty, "0.00000001")]]

    describe "Diff.diff" $ do
        it "can diff nested values" $ do
            -- Based on data from
            -- http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua.
            v1 <- readFile "test/weather1.json"
            v2 <- readFile "test/weather2.json"
            let diff = JD.diff (JD.flatten JP.empty $ decodeText $ T.pack v1)
                               (JD.flatten JP.empty $ decodeText $ T.pack v2)
            let weather1 = JP.addInt 1 $ JP.addText "weather" $ JP.empty
            diff `shouldBe` (M.fromList
                               [ (JP.addText "id" weather1, "520")
                               , (JP.addText "description" weather1,
                                  "light intensity shower rain")
                               , (JP.addText "main" weather1, "Rain")
                               , (JP.addText "icon" weather1, "09d")
                               ],
                               M.fromList [(JP.addText "name" JP.empty,
                                           ("Kiev", "Kyiv"))],
                               M.empty)

    describe "Diff.formatDiff" $ do
        it "can format Diffs" $ do
            let
                v1 =
                    decodeText
                        "{\"k1\": \"value\", \"k2\": {\"nested key\": \
                \[1, {\"deep\": \"space\"}]}, \"L\": 0}"
            let
                v2 =
                    decodeText
                        "{\"k1\": \"value\", \"k2\": {\"nested key\": \
                \[1, {\"deep\": \"sp@ce\"}]}, \"R\": 1}"
            let expectedDiff =
                    [ ".k2['nested key'].1.deep: space -> sp@ce"
                    , "- .L: 0"
                    , "+ .R: 1"
                    ]
            JD.formatDiff (JD.diff (JD.flatten JP.empty v1)
                                   (JD.flatten JP.empty v2))
                `shouldBe` expectedDiff
