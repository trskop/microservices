{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
module Microservices
  where



import Control.Applicative (Applicative((<*>), pure))
import Control.Concurrent (ThreadId, forkIO)
import Data.Function
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, SomeSymbol(SomeSymbol), Symbol)
import System.IO (IO)

-- {{{ Test
import Data.Bool (Bool(True))
import System.IO (print)
import Text.Show (Show)
import Data.Default.Class (Default(def))
import Data.OverloadedLabels.TH
import Data.OverloadedRecords.TH (overloadedRecord)
-- }}} Test

import Data.OverloadedRecords -- TODO: Explicit import list.


type ServiceMain r m name params = Proxy name -> Rec params r -> m ()

-- | Represents one service.
data Svc (r :: *) (m :: * -> *) (name :: Symbol) (params :: [(Symbol, *)])
  where
    Svc :: KnownSymbol name
        => Proxy name
        -> ServiceMain r m name params
        -> Svc r m name params

-- | Smart constructor for 'Svc'.
svc :: KnownSymbol name => ServiceMain r m name params -> Svc r m name params
svc f = Svc Proxy f

-- | Run service using provided configuration. This function is useful for
-- debugging.
runSvc
    :: R params cfg
    => Svc cfg m name params
    -> cfg
    -> m ()
runSvc (Svc svcName f) cfg = f svcName (Rec cfg)

-- | Collection of services.
data Services r m (names :: [Symbol]) (params :: [[(Symbol, *)]]) where
    NoServices :: Services r m '[] '[]

    ServiceIsNotImplemented
        :: KnownSymbol name
        => Proxy name
        -> Proxy ps
        -> Services r m names params
        -> Services r m (name ': names) (ps ': params)

    AService
        :: KnownSymbol name
        => Proxy name
        -> ServiceMain r m name ps
        -> Services r m names params
        -> Services r m (name ': names) (ps ': params)

-- | Union of record constraints.
type family UnionR (cs :: [[(Symbol, *)]]) (r :: *) where
    UnionR '[]       r = R '[] r
    UnionR (c ': cs) r = (R c r, UnionR cs r)

class RunServices e where
    type RuntimeConfiguration e :: *
    type ExecutionMonad e :: * -> *

    foldServices
        :: UnionR params (RuntimeConfiguration e)
        => Services (RuntimeConfiguration e) (ExecutionMonad e) names params
        -> RuntimeConfiguration e
        -> e

runServices :: RunServices e => RuntimeConfiguration e -> e
runServices = foldServices NoServices

instance
    ( RunServices b
    , RuntimeConfiguration b ~ r
    , ExecutionMonad b ~ m
    , R params r
    ) => RunServices (Svc r m name params -> b)
  where
    type RuntimeConfiguration (Svc r m name params -> b) = r
    type ExecutionMonad (Svc r m name params -> b) = m

    foldServices svcs cfg (Svc svcName svcMain) =
        foldServices (AService svcName svcMain svcs) cfg

-- {{{ ForkIO -----------------------------------------------------------------

newtype ForkIO (r :: *) = ForkIO
    { runForkIO :: IO [(SomeSymbol, Maybe ThreadId)]
    }

instance RunServices (ForkIO r) where
    type RuntimeConfiguration (ForkIO r) = r
    type ExecutionMonad (ForkIO r) = IO

    foldServices svcs cfg = ForkIO go
      where
        go = case svcs of
            NoServices -> pure []
            AService svcName svcMain svcs' ->
                (mkResult svcName . Just)
                    <$> forkIO (svcMain svcName (Rec cfg))
                    <*> runForkIO (foldServices svcs' cfg)
            ServiceIsNotImplemented svcName _ svcs' ->
                mkResult svcName Nothing <$> runForkIO (foldServices svcs' cfg)

        mkResult
            :: KnownSymbol name
            => Proxy name
            -> Maybe ThreadId
            -> [(SomeSymbol, Maybe ThreadId)]
            -> [(SomeSymbol, Maybe ThreadId)]
        mkResult svcName possiblyThreadId = ((SomeSymbol svcName, possiblyThreadId) :)

-- {{{ Test -------------------------------------------------------------------

data BaseUrl = BaseUrl deriving Show
data RestCredentials = RestCredentials deriving Show
data Logger = Logger deriving Show

type ApiClientRuntimeConfig =
    '[ "restBaseUrl" ::: BaseUrl
    , "restCredentials" ::: RestCredentials
    , "logger" ::: Logger
    ]

type ApiClient r =
    Svc r IO "ApiClient" ApiClientRuntimeConfig

data C = C
    { _restBaseUrl :: BaseUrl
    , _restCredentials :: RestCredentials
    , _logger :: Logger
    , _verbose :: Bool
    }

overloadedRecord def ''C

labels ["restBaseUrl", "restCredentials", "logger", "verbose"]

apiClient :: ApiClient r
apiClient = svc $ \_svcName (Rec cfg) -> do
    print (get restBaseUrl cfg)
    print (get restCredentials cfg)
    print (get logger cfg)

type Logging r = Svc r IO "Logging" '["logger" ::: Logger, "verbose" ::: Bool]

loggingService :: Logging r
loggingService = svc $ \_svcName (Rec cfg) -> do
    print (get logger cfg)
    print (get verbose cfg)

test :: IO ()
test = do
    r <- runForkIO $ runServices cfg
        loggingService
        apiClient
    print r
  where
    cfg = C BaseUrl RestCredentials Logger True

-- }}} Test -------------------------------------------------------------------
