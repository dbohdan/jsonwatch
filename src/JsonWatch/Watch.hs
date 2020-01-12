-- Copyright (c) 2014, 2015, 2017, 2018, 2019 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module JsonWatch.Watch (watch, watch') where

import qualified JsonWatch.Diff       as JD
import           JsonWatch.Format     (prettyPrint)
import qualified JsonWatch.Path       as JP

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (when)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Time.Format     as F
import           Data.Time.LocalTime  (getZonedTime)

timeFormat = "%Y-%m-%dT%H:%M:%S%z"


empty :: A.Value
empty = fromJust $ A.decode "{}"


timestamp :: IO String
timestamp = do
    now <- getZonedTime
    return $ F.formatTime F.defaultTimeLocale timeFormat now


printTimestamped :: Bool -> T.Text -> IO ()
printTimestamped _ "" = return ()

printTimestamped withTimestamp text = do
    timeStr <- timestamp

    let (sep, indented) = case T.lines text of
            [line] -> (" ", line)
            lines -> ("\n", indent "    " lines)

        timestamped =
            if withTimestamp
            then T.concat [T.pack timeStr, sep, indented]
            else indented

    TIO.putStrLn timestamped
  where
    indent :: T.Text -> [T.Text] -> T.Text
    indent prefix lines =
        T.unlines $ fmap (\line -> T.append prefix line) lines


watch :: Int -> Bool -> Bool -> IO BS.ByteString -> IO ()
watch interval withTimestamp printInitial input = do
    initialStr <- input

    case A.decode initialStr of
        Just initial -> do
            when printInitial $ prettyPrint initial
            watch' interval withTimestamp initial input
        Nothing -> do
            threadDelay $ interval * 1000000
            watch interval withTimestamp printInitial input

    return ()


watch' :: Int -> Bool -> A.Value -> IO BS.ByteString -> IO ()
watch' interval withTimestamp base input = do
    jsonStr <- input

    nextBase <- case A.decode jsonStr of
        Just json -> do
            printTimestamped withTimestamp .
                T.unlines .
                JD.formatDiff $
                JD.diff JP.empty base json
            return json
        Nothing -> return base

    threadDelay $ interval * 1000000

    watch' interval withTimestamp nextBase input
