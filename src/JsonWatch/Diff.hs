-- Copyright (c) 2014, 2015, 2017, 2018, 2019 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Diff (Diff, empty, diff, formatDiff) where

import qualified JsonWatch.Format         as JF
import qualified JsonWatch.Path           as JP

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.HashMap.Lazy        as HM
import qualified Data.List                as L
import qualified Data.Maybe               as M
import qualified Data.Monoid              as MN
import qualified Data.Scientific          as Sci
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Data.Vector.Distance     as VD

data DiffOp
    = Add JP.Path A.Value
    | Change JP.Path A.Value A.Value
    | Remove JP.Path A.Value
    deriving (Eq, Show)

data Diff = Diff [DiffOp] deriving (Eq, Show)


empty :: Diff
empty = Diff []


diff :: JP.Path -> A.Value -> A.Value -> Diff
diff path (A.Object xs) (A.Object ys) =
    Diff $ removed ++ added ++ changed
  where
    foldDiff f as bs = HM.foldlWithKey'
                           (\diffops k v -> (f (JP.add k path) v):diffops)
                           [] $ 
                       HM.difference as bs
    added = foldDiff Add ys xs
    removed = foldDiff Remove xs ys

    changed = HM.foldlWithKey'
                  (\diffops k (Diff nestedDiffOps) -> nestedDiffOps ++ diffops)
                  ([] :: [DiffOp]) $
              HM.intersectionWithKey
                  (\k v1 v2 -> diff (JP.add k path) v1 v2)
                  xs
                  ys

diff path (A.Array xs) (A.Array ys) =
    Diff diffops
  where
    (_, edits) = VD.leastChanges valueDist xs ys

    applyAll fs x = L.foldl' (\x f -> f x) x fs

    f (corrs, acc) ("delete", i, v) =
      ( corrs ++ [\j -> if j >= i then j + 1 else j]
      , (Remove (JP.add (applyAll corrs i) path) v):acc
      )

    f (corrs, acc) ("insert", i, v) =
      ( corrs ++ [\j -> if j >= i then j - 1 else j]
      , (Add (JP.add (applyAll corrs i) path) v):acc
      )

    f (corrs, acc) ("substitute", i, v) =
      ( corrs
      , (Change (JP.add (applyAll corrs i) path) v v):acc
      )

    diffops =
      let (_, corrected) = L.foldl' f ([], [Add path $ A.toJSON $ show edits]) edits 
      in L.reverse corrected

diff path a b
    | a == b = Diff []
    | a /= b = Diff [Change path a b]



valueDist :: VD.Params A.Value (String, Int, A.Value) (MN.Sum Int)
valueDist = VD.Params
    { VD.equivalent = (==)
    , VD.delete     = \i v -> ("delete", i, v)
    , VD.insert     = \i v -> ("insert", i, v)
    , VD.substitute = \i v v' -> ("substitute", i, v')
    , VD.cost = const (MN.Sum 1)
    , VD.positionOffset = \(op, _, _) -> if op == "delete" then 0 else 1
    }

-- flatten prefix (A.Object xs) =
--     M.unions [ flatten (JP.add (k :: Text) prefix) v | (k, v) <- HM.toList xs ]
-- flatten prefix (A.Array xs) = M.unions
--     [ flatten (JP.add (k :: Int) prefix) v
--     | (k, v) <- Prelude.zip [0 ..] $ V.toList xs
--     ]


formatDiff :: Diff -> [T.Text]
formatDiff d = [T.pack . show $ d]

-- formatDiff :: Diff -> [T.Text]
-- formatDiff (Diff as cs rs) =
--     [ T.concat ["- ", JP.formatPath k, ": ", JF.pretty v]
--     | (k, v) <- rs
--     ] ++
--     [ T.concat ["+ ", JP.formatPath k, ": ", JF.pretty v]
--     | (k, v) <- as
--     ]

-- formatDiff :: Diff -> [Text]
-- formatDiff (fromOnly, both, toOnly) =
--     [ T.concat [JP.toText k, ": ", v1, " -> ", v2]
--     | (k, (v1, v2)) <- toAscList both
--     ] ++
--     [ T.concat ["- ", JP.toText k, ": ", v]
--     | (k, v) <- toAscList fromOnly
--     ] ++
--     [ T.concat ["+ ", JP.toText k, ": ", v]
--     | (k, v) <- toAscList toOnly
--     ]

-- formatNumber :: Sci.Scientific -> Text
-- formatNumber x =
--     T.pack $ Sci.formatScientific Sci.Fixed
--                                 (if Sci.isInteger x then Just 0 else Nothing)
--                                 x

-- flatten :: JP.Path -> A.Value -> FlatJson
-- flatten prefix (A.Number x) =
--     M.singleton prefix $ formatNumber x
-- flatten prefix (A.String x    ) = M.singleton prefix x
-- flatten prefix (A.Bool   True ) = M.singleton prefix "true"
-- flatten prefix (A.Bool   False) = M.singleton prefix "false"
-- flatten prefix A.Null           = M.singleton prefix "null"
-- flatten prefix (A.Object xs) =
--     M.unions [ flatten (JP.add (k :: Text) prefix) v | (k, v) <- HM.toList xs ]
-- flatten prefix (A.Array xs) = M.unions
--     [ flatten (JP.add (k :: Int) prefix) v
--     | (k, v) <- Prelude.zip [0 ..] $ V.toList xs
--     ]

-- toAscList :: M.Map JP.Path a -> [(JP.Path, a)]
-- toAscList m = M.toAscList $ M.mapKeys reverse m
