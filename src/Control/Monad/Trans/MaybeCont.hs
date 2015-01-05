{-|
Module      : Control.Monad.Trans.MaybeContT
Description : The 'MaybeContT' type and API
Copyright   : (c) Eitan Chatav, 2015
License     : PublicDomain
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental

The 'MaybeContT' type and API provide an idiomatic way to handle possibly
failing computations in continuation passing style over some base monad.
-}

module Control.Monad.Trans.MaybeCont
    ( MaybeContT
        ( MaybeContT
        , runMaybeContT
        )
    , liftMaybeT
    , nothingC
    , mapMaybeContT
    , withMaybeContTJust
    , withMaybeContTNothing
    ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- |The 'MaybeContT' 'a' 'm' 'r' type encodes a nullable monad transformer
-- in continuation passing style.
newtype MaybeContT a m r
    -- |Construct a continuation-passing computation from a function.
    = MaybeContT
    { -- |The result of running a CPS computation with given nothing and
      -- just continuations.
      runMaybeContT :: m a -> (r -> m a) -> m a
    }

-- |'liftMaybeT' embeds 'MaybeT' in 'MaybeContT' 'a'.
liftMaybeT :: Monad m => MaybeT m r -> MaybeContT a m r
liftMaybeT may = MaybeContT $ \ma k -> maybe ma k =<< runMaybeT may

-- |The 'Functor' instance encodes functoriality of 'MaybeContT' 'a' 'm'.
instance Functor (MaybeContT a m) where
    fmap f mc = MaybeContT $ \ma k -> runMaybeContT mc ma (k . f)

-- |The 'Applicative' instance encodes applicativity of 'MaybeContT' 'a' 'm'.
instance Applicative (MaybeContT a m) where
    pure r = MaybeContT $ \_ k -> k r
    mcf <*> mc = MaybeContT $ \ma k ->
        runMaybeContT mcf ma (\f -> runMaybeContT mc ma (k . f))

-- |The 'Monad' instance encodes monadicity of 'MaybeContT' 'a' 'm'.
instance Monad (MaybeContT a m) where
    return = pure
    mc >>= mcf = MaybeContT $ \ma k ->
        runMaybeContT mc ma (\r -> runMaybeContT (mcf r) ma k)

-- |'MaybeContT' 'a' is a monad transformer.
instance MonadTrans (MaybeContT a) where
    lift mr = MaybeContT $ \_ k -> mr >>= k

-- |Call with current just continuation.
instance MonadCont (MaybeContT a m) where
    callCC f = MaybeContT $ \ma k ->
        runMaybeContT (f (\r -> (MaybeContT $ \_ _ -> k r))) ma k

-- |'nothingC' is the CPS representation of 'Nothing'.
nothingC :: MaybeContT a m r
nothingC = MaybeContT $ \ma _ -> ma

-- |Apply a function to transform the result of a continuation-passing
-- computation.
mapMaybeContT :: (m a -> m a) -> MaybeContT a m r -> MaybeContT a m r
mapMaybeContT f mc = MaybeContT $ \ma k -> f (runMaybeContT mc ma k)

-- |Apply a function to transform the just continuation passed to a
-- continuation-passing computation.
withMaybeContTJust :: ((r' -> m a) -> r -> m a)
                   -> MaybeContT a m r
                   -> MaybeContT a m r'
withMaybeContTJust f mc = MaybeContT $ \ma k -> runMaybeContT mc ma (f k)

-- |Apply a function to transform the nothing continuation passed to a
-- continuation-passing computation.
withMaybeContTNothing :: (m a -> m a) -> MaybeContT a m r -> MaybeContT a m r
withMaybeContTNothing f mc = MaybeContT $ \ma k -> runMaybeContT mc (f ma) k
