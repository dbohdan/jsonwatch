-- Copyright (c) 2014, 2015, 2017 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Watch (watch) where

import           JsonWatch.Diff             as JD

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (when)
import           Data.Aeson                 as A
import           Data.ByteString.Lazy.Char8 as C8
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  as T
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           as F
import           Data.Time.LocalTime        (getZonedTime)

timeFormat = "%Y-%m-%dT%H:%M:%S%z"

timestamp :: IO String
timestamp = do
    now <- getZonedTime
    return $ F.formatTime F.defaultTimeLocale timeFormat now

printDiff :: Bool -> [Text] -> IO ()
printDiff _     []     = return ()
printDiff False [line] = do
    Prelude.putStrLn $ T.unpack line
printDiff False lines  = do
    Prelude.putStrLn $ T.unpack $ T.unlines lines
printDiff True  [line] = do
    timeStr <- timestamp
    Prelude.putStrLn $ timeStr ++ " " ++ T.unpack line
printDiff True  lines  = do
    timeStr <- timestamp
    let text = T.unlines [ T.append "    " x | x <- lines ]
    Prelude.putStrLn $ timeStr ++ "\n" ++ T.unpack text

watch :: Int -> Bool -> Bool -> Maybe JD.FlatJson -> IO String -> IO ()
watch interval date print prev thunk = do
    jsonStr <- thunk
    let decoded = A.decode $ C8.pack jsonStr
    -- Only print valid JSON.
    case decoded of
        Just value -> when print $ Prelude.putStrLn jsonStr
        Nothing    -> return ()
    let jsonFlat = JD.flatten "" $ fromMaybe (A.toJSON ()) decoded
    case prev of
        Just prevFlat ->
            printDiff date $ JD.formatDiff $ JD.diff prevFlat jsonFlat
        Nothing -> return ()
    threadDelay $ interval * 1000000
    watch interval date False (Just jsonFlat) thunk
