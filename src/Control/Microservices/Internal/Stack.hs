{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Control.Microservices.Internal.Stack
    (
    -- * Service
      ServiceMain
    , Svc(..)
    , svc
    , getSvcName
    , runSvc

    -- * Service Stack
    , ServiceStack(..)
    , HasService(..)
    , HasServices
    , peek
    , peek_
    , peekSvcName
    , pop
    , pop_
    , push
    , push_

    -- ** Internals
    , HasService'(..)
    , Position
    )
  where

import Data.Bool (Bool(False, True))
import Data.Either (Either(Left, Right))
import Data.Function ((.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality (type (==))
import GHC.Exts (Constraint)
import GHC.TypeLits (type (-), type (+), Nat, KnownSymbol, Symbol)

import Data.Default.Class (Default(def))
import Data.OverloadedRecords (R, Rec(Rec))


type ServiceMain r m name params = Proxy name -> Rec params r -> m ()

-- | Represents one service named @name :: Symbol@ that accepts @params ::
-- [(Symbol, *)]@ in the form of overloaded record @r :: *@, and returns action
-- in the monad @m :: * -> *@.
data Svc (r :: *) (m :: * -> *) (name :: Symbol) (params :: [(Symbol, *)])
  where
    -- | *Using 'Svc' constructor directly is considered unsafe.*
    Svc :: KnownSymbol name
        => ServiceMain r m name params
        -> Svc r m name params

-- | Constructor for 'Svc'.
svc :: KnownSymbol name => ServiceMain r m name params -> Svc r m name params
svc = Svc
{-# INLINE svc #-}

-- | Get service name in the form of string type literal @name :: Symbol@.
--
-- Use following to get service name in form of a 'Data.String.String':
--
-- @
-- 'GHC.TypeLits.symbolVal' . 'getSvcName'
--     :: 'Svc' r m name params -> 'Data.String.String'
-- @
getSvcName :: Svc r m name params -> Proxy name
getSvcName _svc = Proxy
{-# INLINE getSvcName #-}

-- | Run service using provided configuration.
runSvc
    :: R params cfg
    => Svc cfg m name params
    -> cfg
    -> m ()
runSvc s@(Svc f) cfg = f (getSvcName s) (Rec cfg)
{-# INLINE runSvc #-}

-- | Collection of services.
data ServiceStack r m (names :: [Symbol]) (paramss :: [[(Symbol, *)]]) where
    -- | Empty service stack, i.e. one without any services in it.
    --
    -- *Using 'Empty' constructor directly is considered unsafe.*
    Empty :: ServiceStack r m '[] '[]

    -- | Like 'Cons', but there is no implementation for a service.
    --
    -- *Using 'Next' constructor directly is considered unsafe.*
    Next
        :: KnownSymbol name
        => ServiceStack r m names paramss
        -> ServiceStack r m (name ': names) (params ': paramss)

    -- | Push service in to a stack.
    --
    -- *Using 'Cons' constructor directly is considered unsafe.*
    Push
        :: KnownSymbol name
        => ServiceMain r m name params
        -> ServiceStack r m names paramss
        -> ServiceStack r m (name ': names) (params ': paramss)

instance Default (ServiceStack r m '[] '[]) where
    def = Empty

-- | Take top most element of a stack and return it along with a new stack with
-- it removed.
pop :: ServiceStack r m (name ': names) (params ': paramss)
    -> (ServiceStack r m names paramss, Maybe (Svc r m name params))
pop = \case
    Push svcMain svcs -> (svcs, Just (Svc svcMain))
    Next         svcs -> (svcs, Nothing)
    -- Empty is an imposile case.

-- | Same as 'pop', but just discards the top most element of a stack and
-- returns new stack without it.
pop_
    :: ServiceStack r m (name ': names) (params ': paramss)
    -> ServiceStack r m names paramss
pop_ = \case
    Push _ svcs -> svcs
    Next   svcs -> svcs
    -- Empty is an imposile case.

-- | Peek top most element, if there is one.
peek
    :: ServiceStack r m (name ': names) (params ': paramss)
    -> Either (ServiceStack r m names paramss) (Svc r m name params)
peek = \case
    Push svcMain _    -> Right (Svc svcMain)
    Next         svcs -> Left svcs

-- | Similar to 'peek', but result is in the form of 'Maybe' instead of
-- 'Either'.
peek_
    :: ServiceStack r m (name ': names) (params ': paramss)
    -> Maybe (Svc r m name params)
peek_ = \case
    Push svcMain _ -> Just (Svc svcMain)
    _              -> Nothing

peekSvcName
    :: ServiceStack r m (name ': names) (params ': paramss)
    -> Proxy name
peekSvcName _ = Proxy
{-# INLINE peekSvcName #-}

-- | Push a service in to a stack.
push
    :: Svc r m name params
    -> ServiceStack r m names paramss
    -> ServiceStack r m (name ': names) (params ': paramss)
push (Svc svcMain) = Push svcMain

-- | Push a non-implemented service in to a stack.
push_
    :: KnownSymbol anyName
    => ServiceStack r m names paramss
    -> ServiceStack r m (anyName ': names) (anyParams ': paramss)
push_ = Next

class HasService' (name :: Symbol) (params :: [(Symbol, *)])
    (names :: [Symbol]) (paramss :: [[(Symbol, *)]]) (n :: Nat) (b :: Bool)
  where
    project'
        :: Proxy '(n, b)
        -> ServiceStack r m names paramss
        -> Maybe (Svc r m name params)

instance
    ( names ~ (name ': names')
    , paramss ~ (params ': paramss')
    ) => HasService' name params names paramss 0 'True
  where
    project' _ = peek_

instance
    ( names ~ (name ': names')
    , paramss ~ (params ': paramss')
    , n' ~ (n - 1)
    , HasService' name params names' paramss' n' (n' == 0)
    ) => HasService' name params names paramss n 'False
  where
    project' _ = project' (Proxy :: Proxy '(n', n' == 0)) . pop_

type family Position (name :: Symbol) (params :: [(Symbol, *)])
    (names :: [Symbol]) (paramss :: [[(Symbol, *)]]) :: Nat
  where
    Position n ps (n    ': ns) (ps    ': pss) = 0
    Position n ps (anyn ': ns) (anyps ': pss) = 1 + Position n ps ns pss

class HasService (name :: Symbol) (params :: [(Symbol, *)]) (names :: [Symbol])
    (paramss :: [[(Symbol, *)]])
  where
    project :: ServiceStack r m names paramss -> Maybe (Svc r m name params)

instance
    ( HasService' name params names paramss
        (Position name params names paramss)
        (Position name params names paramss == 0)
    ) => HasService name params names paramss
  where
    project = project' p
      where
        p = Proxy :: Proxy
            '( Position name params names paramss
            , Position name params names paramss == 0
            )

type family HasServices (names :: [Symbol]) (paramss :: [[(Symbol, *)]])
    (names' :: [Symbol]) (paramss' :: [[(Symbol, *)]]) :: Constraint
  where
    HasServices '[]       '[]         ns' pss' = ()
    HasServices (n ': ns) (ps ': pss) ns' pss' =
        ( HasService n ps ns' pss'
        , HasServices ns pss ns' pss'
        )
