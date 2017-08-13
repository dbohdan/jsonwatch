-- Copyright (c) 2014, 2015, 2017 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Diff (DiffMap, Diff, FlatJson, diff, flatten, formatDiff) where

import qualified JsonWatch.Path    as JP

import qualified Data.Aeson        as A
import qualified Data.HashMap.Lazy as HM
import qualified Data.List         as L
import qualified Data.Map.Strict   as M
import qualified Data.Scientific   as S
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Vector       as V

type FlatJson = M.Map JP.Path Text
type DiffMap = M.Map JP.Path (Text, Text)
type Diff = (FlatJson, DiffMap, FlatJson)

diff :: FlatJson -> FlatJson -> Diff
diff from to = (fromOnly, both, toOnly)
  where
    fromOnly = M.difference from to
    toOnly   = M.difference to from
    both     = M.mapMaybe id $ M.intersectionWith f from to
    f v1 v2  = if (v1 :: Text) == v2 then Nothing else Just (v1, v2)

formatDiff :: Diff -> [Text]
formatDiff (fromOnly, both, toOnly) =
    [ T.concat [JP.toText k, ": ", v1, " -> ", v2]
    | (k, (v1, v2)) <- toAscList both
    ] ++
    [ T.concat ["- ", JP.toText k, ": ", v]
    | (k, v) <- toAscList fromOnly
    ] ++
    [ T.concat ["+ ", JP.toText k, ": ", v]
    | (k, v) <- toAscList toOnly
    ]

flatten :: JP.Path -> A.Value -> FlatJson
flatten prefix (A.Number x) =
    M.singleton prefix (T.pack $ S.formatScientific S.Fixed Nothing x)
flatten prefix (A.String x    ) = M.singleton prefix x
flatten prefix (A.Bool   True ) = M.singleton prefix "true"
flatten prefix (A.Bool   False) = M.singleton prefix "false"
flatten prefix A.Null           = M.singleton prefix "null"
flatten prefix (A.Object xs) =
    M.unions [ flatten (JP.add (k :: Text) prefix) v | (k, v) <- HM.toList xs ]
flatten prefix (A.Array xs) = M.unions
    [ flatten (JP.add (k :: Int) prefix) v
    | (k, v) <- Prelude.zip [0 ..] $ V.toList xs
    ]

toAscList :: M.Map JP.Path a -> [(JP.Path, a)]
toAscList m = M.toAscList $ M.mapKeys reverse m
