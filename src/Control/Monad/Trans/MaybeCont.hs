module Control.Monad.Trans.MaybeCont
    ( MaybeContT
        ( MaybeContT
        , runMaybeContT
        )
    , nothingC
    , mapMaybeContT
    , withMaybeContT
    ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Trans

newtype MaybeContT a m r = MaybeContT
    { runMaybeContT :: m a -> (r -> m a) -> m a }

instance Functor (MaybeContT a m) where
    fmap f mc = MaybeContT $ \ma k -> runMaybeContT mc ma (k . f)

instance Applicative (MaybeContT a m) where
    pure r = MaybeContT $ \_ k -> k r
    mcf <*> mc = MaybeContT $ \ma k ->
        runMaybeContT mcf ma (\f -> runMaybeContT mc ma (k . f))

instance Monad (MaybeContT a m) where
    return = pure
    mc >>= mcf = MaybeContT $ \ma k ->
        runMaybeContT mc ma (\r -> runMaybeContT (mcf r) ma k)

instance MonadTrans (MaybeContT a) where
    lift mr = MaybeContT $ \_ k -> mr >>= k

instance MonadCont (MaybeContT a m) where
    callCC f = MaybeContT $ \ma k ->
        runMaybeContT (f (\r -> (MaybeContT $ \_ _ -> k r))) ma k

nothingC :: MaybeContT a m r
nothingC = MaybeContT $ \ma _ -> ma

mapMaybeContT :: (m a -> m a) -> MaybeContT a m r -> MaybeContT a m r
mapMaybeContT f mc = MaybeContT $ \ma k -> f (runMaybeContT mc ma k)

withMaybeContT :: ((r' -> m a) -> r -> m a)
               -> MaybeContT a m r
               -> MaybeContT a m r'
withMaybeContT f mc = MaybeContT $ \ma k -> runMaybeContT mc ma (f k)
