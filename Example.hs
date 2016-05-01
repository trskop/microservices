{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
--               MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
--               TemplateHaskell, TypeFamilies, TypeOperators,
--               UndecidableInstances
--
-- TODO
module Example
  where

import Prelude (Num((-)))

import Control.Applicative (Applicative((<*>), pure))
import Control.Concurrent
    ( Chan
    , MVar
    , killThread
    , newChan
    , newEmptyMVar
    , putMVar
    , readChan
    , readMVar
    , threadDelay
    , writeChan
    )
import Control.Monad (Monad((>>=)), (>>))
import Data.Foldable (mapM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (IO, putStrLn)
import Text.Show (Show(show))

import Data.Default.Class (Default(def))
import Data.OverloadedLabels.TH (labels)
import Data.OverloadedRecords ((:::), R, Rec(Rec), get)
import Data.OverloadedRecords.TH (overloadedRecord)

import Control.Microservices


data Logger = Logger {getLoggerChan :: Chan String}

instance Show Logger where
    show _ = "<<logger>>"

data C = C
    { _timeout :: Int
    , _count :: Int
    , _logger :: Logger
    , _terminate :: MVar ()
    }

overloadedRecord def ''C

labels ["snd", "timeout", "count", "logger", "terminate"]

putLogMsg :: R '["logger" ::: Logger] r => r -> String -> IO ()
putLogMsg = writeChan . getLoggerChan . get logger

-- {{{ Client Service ---------------------------------------------------------

type Client r = Svc r IO "Client"
    '[ "timeout" ::: Int
    , "count" ::: Int
    , "logger" ::: Logger
    , "terminate" ::: MVar ()
    ]

client :: Client r
client = Svc $ \_svcName (Rec cfg) -> do
    let putLogMsg' = putLogMsg cfg . ("Client: " <>)
    putLogMsg' $ "timeout: " <> show (get timeout cfg)
    putLogMsg' $ "count: " <> show (get count cfg)
    putLogMsg' $ "logger: " <> show (get logger cfg)

    loop cfg (get timeout cfg) (get count cfg)
  where
    loop cfg t = \case
        0 -> putLogMsg cfg "Client: Done." >> putMVar (get terminate cfg) ()
        n -> do
            putLogMsg cfg ("Client: Ping: " <> show (get count cfg - n))
            threadDelay t
            loop cfg t (n - 1)

-- }}} Client Service ---------------------------------------------------------

-- {{{ Logging Service --------------------------------------------------------

type Logging r = Svc r IO "Logging" '["logger" ::: Logger]

loggingService :: Logging r
loggingService = Svc $ \_svcName (Rec cfg) -> do
    putLogMsg cfg $ "Logging: logger: " <> show (get logger cfg)
    loop cfg
  where
    loop r = readLogMessage r >>= putStrLn >> loop r
    readLogMessage = readChan . getLoggerChan . get logger

-- }}} Logging Service --------------------------------------------------------

main :: IO ()
main = do
    cfg <- mkCfg
        <$> newLogger
        <*> newEmptyMVar
    threadIds <- runForkIO $ runServices cfg
        loggingService
        client
    putLogMsg cfg $ "Main: " <> show threadIds
    waitToTerminate cfg
    killAll cfg threadIds
  where
    mkCfg = C 100000 10
    newLogger = Logger <$> newChan
    waitToTerminate cfg = readMVar $ get terminate cfg

    killAll cfg tids = do
        -- Wait for logger to finish writing messages.
        threadDelay (get timeout cfg)
        mapM_ (maybe (pure ()) killThread . get snd) tids
