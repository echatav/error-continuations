{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.Trans.EitherCont
    ( EitherContT
        ( EitherContT
        , runEitherContT
        )
    , eitherTEC
    , fmapL
    , bimapEC
    , throwEC
    , apL
    , catchEC
    , liftL
    , flipEC
    , mapEitherContT
    , withEitherContTR
    , withEitherContTL
    , callCCL
    , runInMonadError
    ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

newtype EitherContT a l m r = EitherContT
    { runEitherContT :: (l -> m a) -> (r -> m a) -> m a }

eitherTEC :: Monad m => EitherT l m r -> EitherContT a l m r
eitherTEC e = EitherContT $ \kl kr -> eitherT kl kr e

instance Functor (EitherContT a l m) where
    fmap f ec = EitherContT $ \kl kr -> runEitherContT ec kl (kr . f)

fmapL :: (l -> l') -> EitherContT a l m r -> EitherContT a l' m r
fmapL f ec = EitherContT $ \kl kr -> runEitherContT ec (kl . f) kr

bimapEC :: (l -> l') -> (r -> r')
        -> EitherContT a l m r
        -> EitherContT a l' m r'
bimapEC fl fr ec = EitherContT $ \kl kr -> runEitherContT ec (kl . fl) (kr . fr)

instance Applicative (EitherContT a l m) where
    pure r = EitherContT $ \_ kr -> kr r
    ecf <*> ec = EitherContT $ \kl kr ->
        runEitherContT ecf kl (\f -> runEitherContT ec kl (kr . f))

throwEC :: l -> EitherContT a l m r
throwEC l = EitherContT $ \kl _ -> kl l

apL :: EitherContT a (l -> l') m r
    -> EitherContT a l m r
    -> EitherContT a l' m r
apL ecf ec = EitherContT $ \kl kr ->
    runEitherContT ecf (\f -> runEitherContT ec (kl . f) kr) kr

instance Monad (EitherContT a l m) where
    return = pure
    ec >>= ecf = EitherContT $ \kl kr ->
        runEitherContT ec kl (\r -> runEitherContT (ecf r) kl kr)

catchEC :: EitherContT a l m r
       -> (l -> EitherContT a l' m r)
       -> EitherContT a l' m r
catchEC ec ecf = EitherContT $ \kl kr ->
    runEitherContT ec (\l -> runEitherContT (ecf l) kl kr) kr

handleEC :: (l -> EitherContT a l' m r)
        -> EitherContT a l m r
        -> EitherContT a l' m r
handleEC = flip catchEC

instance MonadTrans (EitherContT a l) where
    lift mr = EitherContT $ \_ kr -> mr >>= kr

liftL :: Monad m => m l -> EitherContT a l m r
liftL ml = EitherContT $ \kl _ -> ml >>= kl

flipEC :: EitherContT a l m r -> EitherContT a r m l
flipEC = EitherContT . flip . runEitherContT

mapEitherContT :: (m a -> m a) -> EitherContT a l m r -> EitherContT a l m r
mapEitherContT f ec = EitherContT $ \kl kr -> f (runEitherContT ec kl kr)

withEitherContTR :: ((r' -> m a) -> r -> m a)
                 -> EitherContT a l m r
                 -> EitherContT a l m r'
withEitherContTR f ec = EitherContT $ \kl kr -> runEitherContT ec kl (f kr)

withEitherContTL :: ((l' -> m a) -> l -> m a)
                 -> EitherContT a l m r
                 -> EitherContT a l' m r
withEitherContTL f ec = EitherContT $ \kl kr -> runEitherContT ec (f kl) kr

instance MonadCont (EitherContT a l m) where
    callCC f = EitherContT $ \kl kr ->
        runEitherContT (f (\r -> (EitherContT $ \_ _ -> kr r))) kl kr

callCCL :: ((l -> EitherContT a l' m r) -> EitherContT a l m r)
        -> EitherContT a l m r
callCCL f = EitherContT $ \kl kr ->
    runEitherContT (f (\l -> (EitherContT $ \_ _ -> kl l))) kl kr

instance MonadError l (EitherContT a l m) where
    throwError = throwEC
    catchError = catchEC

runInMonadError :: MonadError l m => EitherContT r l m r -> m r
runInMonadError ec = runEitherContT ec throwError return
