-- Copyright (c) 2014, 2015, 2017 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Watch (watch) where

import qualified JsonWatch.Diff             as JD
import qualified JsonWatch.Path             as JP

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (when)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as AP
import           Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.UTF8  as UTF8
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Time.Clock            (getCurrentTime)
import qualified Data.Time.Format           as F
import           Data.Time.LocalTime        (getZonedTime)

timeFormat = "%Y-%m-%dT%H:%M:%S%z"

timestamp :: IO String
timestamp = do
    now <- getZonedTime
    return $ F.formatTime F.defaultTimeLocale timeFormat now

printDiff :: Bool -> [T.Text] -> IO ()
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

watch :: Int -> Bool -> Bool -> Maybe JD.FlatJson -> IO BS.ByteString -> IO ()
watch interval date print prev input = do
    jsonStr <- input
    let decoded = A.decode jsonStr
    -- Only print valid JSON.
    case decoded of
        Just value -> when print $ Prelude.putStrLn $ UTF8.toString $
                      AP.encodePretty decoded
        Nothing    -> return ()
    let jsonFlat = JD.flatten JP.empty $ fromMaybe (A.toJSON ()) decoded
    case prev of
        Just prevFlat ->
            printDiff date $ JD.formatDiff $ JD.diff prevFlat jsonFlat
        Nothing -> return ()
    threadDelay $ interval * 1000000
    watch interval date False (Just jsonFlat) input
