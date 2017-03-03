-- Copyright (c) 2014, 2015, 2017 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Diff (FlatJson, DiffMap, Diff, diff, flatten, formatDiff) where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

type FlatJson = HM.HashMap Text Text
type DiffMap = HM.HashMap Text (Text, Text)
type Diff = (FlatJson, DiffMap, FlatJson)

diff :: FlatJson -> FlatJson -> Diff
diff from to =
    (fromOnly, both, toOnly)
  where
    fromOnly = HM.difference from to
    toOnly = HM.difference to from
    both = HM.mapMaybe id $ HM.intersectionWith f from to
    f = \v1 v2 -> if (v1 :: Text) == v2 then Nothing else Just (v1, v2)

formatDiff :: Diff -> [Text]
formatDiff (fromOnly, both, toOnly) =
    [T.concat [k, ": ", v1, " -> ", v2] | (k, (v1, v2)) <- HM.toList both] ++
    [T.concat ["- ", k, ": ", v]        | (k, v) <- HM.toList fromOnly] ++
    [T.concat ["+ ", k, ": ", v]        | (k, v) <- HM.toList toOnly]

flatten :: Text -> A.Value -> FlatJson
flatten prefix (A.Number x) =
    HM.singleton prefix (T.pack $ S.formatScientific S.Fixed Nothing x)
flatten prefix (A.String x) =
    HM.singleton prefix x
flatten prefix (A.Bool True) =
    HM.singleton prefix "true"
flatten prefix (A.Bool False) =
    HM.singleton prefix "false"
flatten prefix A.Null =
    HM.singleton prefix "null"
flatten prefix (A.Object xs) =
    HM.unions [flatten (mergePath prefix k) v | (k, v) <- HM.toList xs]
flatten prefix (A.Array xs) =
    HM.unions [flatten (mergePath prefix $ T.pack $ show k) v |
               (k, v) <- Prelude.zip [0..] $ V.toList xs]

mergePath :: Text -> Text -> Text
mergePath a b =
    case T.find (\c -> c == ' ') b of -- TODO: Detect other whitespace?
        Just _  -> T.concat [a, "['", b, "']"] -- Doesn't escape "'" in b.
        Nothing -> T.concat [a, ".", b]
