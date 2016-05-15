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
-- Portability:  GHC specific language extensions.
--
-- TODO
module Control.Microservices
  where

import Control.Applicative (Applicative((<*>), pure))
import Control.Concurrent (ThreadId, forkIO)
import Data.Function
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy{-(Proxy)-})
import GHC.TypeLits (KnownSymbol, SomeSymbol(SomeSymbol), Symbol)
import System.IO (IO)

import Data.OverloadedRecords (R, Rs, Rec(Rec))

import Control.Microservices.Internal.Stack (HasServices, ServiceStack, Svc(..))
import qualified Control.Microservices.Internal.Stack as Stack
    ( ServiceStack(Empty, Next, Push)
    , peekSvcName
    )

class RunServices e where
    type RuntimeConfiguration e :: *
    type ExecutionMonad e :: * -> *

    foldServices
        :: Rs params (RuntimeConfiguration e)
        => ServiceStack (RuntimeConfiguration e) (ExecutionMonad e) names params
        -> RuntimeConfiguration e
        -> e

runServices :: RunServices e => RuntimeConfiguration e -> e
runServices = foldServices Stack.Empty

instance
    ( RunServices b
    , RuntimeConfiguration b ~ r
    , ExecutionMonad b ~ m
    , R params r
    ) => RunServices (Svc r m name params -> b)
  where
    type RuntimeConfiguration (Svc r m name params -> b) = r
    type ExecutionMonad (Svc r m name params -> b) = m

    foldServices svcs cfg (Svc svcMain) =
        foldServices (Stack.Push svcMain svcs) cfg

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
    => ServiceStack r IO names ps
    -> r
    -> ForkIO r
foldForkIO s = ForkIO . loop s
  where
    loop
        :: Rs ps r
        => ServiceStack r IO names ps
        -> r
        -> IO [(SomeSymbol, Maybe ThreadId)]
    loop svcs cfg = case svcs of
        Stack.Empty -> pure []
        Stack.Push svcMain svcs' -> let svcName = Stack.peekSvcName svcs in
            (mkResult svcName . Just)
                -- Consider using "forkFinally" construct.
                <$> forkIO (svcMain svcName (Rec cfg))
                <*> loop svcs' cfg
        Stack.Next svcs' -> let svcName = Stack.peekSvcName svcs in
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

type family Zip (xs :: [k1]) (ys :: [k2]) :: [(k1, k2)] where
    Zip '[]       '[]       = '[]
    Zip (x ': xs) (y ': ys) = '(x, y) ': Zip xs ys

type family Unzip (ps :: [(k1, k2)]) :: ([k1], [k2]) where
    Unzip '[]       = '( '[], '[] )
    Unzip (p ': ps) = Unzip' p (Unzip ps)

type family Unzip' (p :: (k1, k2)) (ps :: ([k1], [k2])) :: ([k1], [k2]) where
    Unzip' '(x, y) '(xs, ys) = '(x ': xs, y ': ys)

-- | Run services from service stack, note that it is not necessary to run all
-- of them, but it has to be a subset.
class ServiceStackRunner a where
    type RuntimeConfiguration' a :: *
    type ExecutionMonad' a :: * -> *
    type ExecutedServices a :: ([Symbol], [[(Symbol, *)]])

    runServiceStack
        ::  ( '(names, paramss) ~ ExecutedServices a
            , HasServices names paramss stackNames stackParamss
            , r ~ RuntimeConfiguration' a
            , m ~ ExecutionMonad' a
            , Rs paramss r
            )
        => r
        -> ServiceStack r m stackNames stackParamss
        -> a

{- TODO: Instance ServiceStackRunner for ForkIO. This is just an idea and
 -       ForkIO may be redesigned to accommodate.
instance
    ServiceStackRunner
        (Proxy ('(names, paramss) :: ([Symbol], [[(Symbol, *)]])) -> ForkIO r)
  where
    type RuntimeConfiguration' (Proxy '(names, paramss) -> ForkIO r) = r
    type ExecutionMonad' (Proxy '(names, paramss) -> ForkIO r) = IO
    type ExecutedServices (Proxy '(names, paramss) -> ForkIO r) = '(names, paramss)

    runServiceStack = let x = x in x
-}
