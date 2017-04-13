-- Copyright (c) 2014, 2015, 2017 dbohdan
-- This code is released under the MIT license. See the file LICENSE.
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified JsonWatch.Watch            as JW

import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Semigroup             ((<>))
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Client.TLS    as HCT
import           Options.Applicative
import qualified System.Process             as P

data Source
  = Command String
  | Url String deriving Show

data WatchOpts = WatchOpts
  { source          :: Source
  , interval        :: Int
  , noDate          :: Bool
  , noInitialValues :: Bool } deriving Show

commandSourceOpt :: Parser Source
commandSourceOpt = Command <$> strOption
    ( long "command" <> short 'c' <> metavar "command" <> help
        "Command to execute"
    )

urlSourceOpt :: Parser Source
urlSourceOpt =
    Url
        <$> strOption
                ( long "url" <> short 'u' <> metavar "url" <> help
                    "URL to fetch"
                )

sourceOpt :: Parser Source
sourceOpt = commandSourceOpt <|> urlSourceOpt

watchOpts :: Parser WatchOpts
watchOpts =
    WatchOpts
        <$> sourceOpt
        <*> option
                auto
                (  long "interval"
                <> short 'n'
                <> metavar "seconds"
                <> value 5
                <> help "Polling interval"
                )
        <*> switch
                (  long "no-date"
                <> help "Don't print date and time for each diff"
                )
        <*> switch
                (  long "no-initial-values"
                <> help "Don't print initial JSON values"
                )

main :: IO ()
main = start =<< execParser opts
 where
  opts = info
      (watchOpts <**> helper)
      ( fullDesc <> progDesc "Track changes in JSON data" <> header
          "jsonwatch v0.3.0"
      )

httpGet :: String -> IO String
httpGet url = do
    manager  <- HC.newManager HCT.tlsManagerSettings
    request  <- HC.parseRequest url
    response <- HC.httpLbs request manager
    return $ C8.unpack $ HC.responseBody response

start :: WatchOpts -> IO ()
start (WatchOpts source interval noDate noInitialValues) =
    JW.watch interval (not noDate) (not noInitialValues) Nothing thunk
 where
  thunk = case source of
      Command command -> P.readCreateProcess (P.shell command) ""
      Url     url     -> httpGet url
