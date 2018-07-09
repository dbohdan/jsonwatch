-- Copyright (c) 2014, 2015, 2017, 2018 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Path (Path, PathElement, add, addInt, addText, empty,
                       toText) where

import qualified Data.Text as T

data PathElement = IntIndex Int | TextIndex T.Text deriving (Eq, Ord, Show)
type Path = [PathElement]

class CanAddToPath a where
    add :: a -> Path -> Path

instance CanAddToPath Int where
    add = addInt

instance CanAddToPath T.Text where
    add = addText

addInt :: Int -> Path -> Path
addInt i pels = IntIndex i : pels

addText :: T.Text -> Path -> Path
addText t pels = TextIndex t : pels

empty :: Path
empty = []

toText :: Path -> T.Text
toText pels = T.concat $ fmap f pels
  where
    f (IntIndex i) = T.append "." $ T.pack $ show i
    f (TextIndex t) = case T.find (\c -> c == ' ') t of
        -- TODO: Detect other whitespace?
        Just _  -> T.concat ["['", t, "']"] -- Doesn't escape "'" in b.
        Nothing -> T.append "." t
