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
module Control.Microservices
  where

import Control.Applicative (Applicative((<*>), pure))
import Control.Concurrent (ThreadId, forkIO)
import Data.Function
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, SomeSymbol(SomeSymbol), Symbol)
import System.IO (IO)

import Data.OverloadedRecords (R, Rec(Rec))


type ServiceMain r m name params = Proxy name -> Rec params r -> m ()

-- | Represents one service.
data Svc (r :: *) (m :: * -> *) (name :: Symbol) (params :: [(Symbol, *)])
  where
    Svc :: KnownSymbol name
        => ServiceMain r m name params
        -> Svc r m name params

getSvcName :: Svc r m name params -> Proxy name
getSvcName _svc = Proxy

-- | Run service using provided configuration. This function is useful for
-- debugging.
runSvc
    :: R params cfg
    => Svc cfg m name params
    -> cfg
    -> m ()
runSvc svc@(Svc f) cfg = f (getSvcName svc) (Rec cfg)

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

-- | Union\/concatenation of record constraints.
type family Rs (cs :: [[(Symbol, *)]]) (r :: *) where
    Rs '[]       r = R '[] r
    Rs (c ': cs) r = (R c r, Rs cs r)

class RunServices e where
    type RuntimeConfiguration e :: *
    type ExecutionMonad e :: * -> *

    foldServices
        :: Rs params (RuntimeConfiguration e)
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

    foldServices svcs cfg svc@(Svc svcMain) =
        foldServices (AService (getSvcName svc) svcMain svcs) cfg

-- {{{ ForkIO -----------------------------------------------------------------

newtype ForkIO (r :: *) = ForkIO
    { runForkIO :: IO [(SomeSymbol, Maybe ThreadId)]
    }

instance RunServices (ForkIO r) where
    type RuntimeConfiguration (ForkIO r) = r
    type ExecutionMonad (ForkIO r) = IO

    foldServices = foldForkIO

foldForkIO
    :: Rs ps r
    => Services r IO names ps
    -> r
    -> ForkIO r
foldForkIO s = ForkIO . loop s
  where
    loop
        :: Rs ps r
        => Services r IO names ps
        -> r
        -> IO [(SomeSymbol, Maybe ThreadId)]
    loop svcs cfg = case svcs of
        NoServices -> pure []
        AService svcName svcMain svcs' ->
            (mkResult svcName . Just)
                -- Consider using "forkFinally" construct.
                <$> forkIO (svcMain svcName (Rec cfg))
                <*> loop svcs' cfg
        ServiceIsNotImplemented svcName _ svcs' ->
            mkResult svcName Nothing <$> loop svcs' cfg
      where
        mkResult
            :: KnownSymbol name
            => Proxy name
            -> Maybe ThreadId
            -> [(SomeSymbol, Maybe ThreadId)]
            -> [(SomeSymbol, Maybe ThreadId)]
        mkResult svcName possiblyThreadId =
            ((SomeSymbol svcName, possiblyThreadId) :)

-- }}} ForkIO -----------------------------------------------------------------
