{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Control.Monad.Trans.EitherCont
Description : The 'EitherContT' type and API
Copyright   : (c) Eitan Chatav, 2015
License     : PublicDomain
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental

The 'EitherContT' type and API provide an idiomatic way to handle errors in
continuation passing style over some base monad.
-}

module Control.Monad.Trans.EitherCont
    ( EitherContT
        ( EitherContT
        , runEitherContT
        )
    , liftEitherT
    , fmapL
    , bimapEC
    , throwEC
    , apL
    , catchEC
    , handleEC
    , (<?<)
    , (>?>)
    , liftL
    , flipEC
    , mapEitherContT
    , withEitherContTR
    , withEitherContTL
    , callCCL
    , lowerMonadError
    , liftMonadError
    ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either

-- |The 'EitherContT' 'a' 'l' 'm' 'r' type encodes a sum type monad transformer
-- in continuation passing style which is separately monadic in both 'l' and
-- 'r'. Interestingly, this property holds for any type constructor 'm'.
newtype EitherContT a l m r
    -- |Construct a continuation-passing computation from a function.
    = EitherContT
    { -- |The result of running a CPS computation with given failure and
      -- success continuations.
      runEitherContT :: (l -> m a) -> (r -> m a) -> m a
    }

-- |'liftEitherT' embeds 'EitherT' in 'EitherContT' 'a'.
liftEitherT :: Monad m => EitherT l m r -> EitherContT a l m r
liftEitherT e = EitherContT $ \kl kr -> eitherT kl kr e

-- |The 'Functor' instance encodes functoriality of 'EitherContT' 'a' 'l' 'm'
-- 'r' in 'r'.
instance Functor (EitherContT a l m) where
    fmap f ec = EitherContT $ \kl kr -> runEitherContT ec kl (kr . f)

-- |'fmapL' encodes functoriality of 'EitherContT' 'a' 'l' 'm' 'r' in 'l'.
fmapL :: (l -> l') -> EitherContT a l m r -> EitherContT a l' m r
fmapL f ec = EitherContT $ \kl kr -> runEitherContT ec (kl . f) kr

-- |'bimapEC' encodes bifunctoriality of 'EitherContT' 'a' 'l' 'm' 'r' in 'l'
-- and 'r'.
--
-- prop> bimapEC f id = fmapL f
-- prop> bimapEC id f = fmap f
bimapEC :: (l -> l') -> (r -> r')
        -> EitherContT a l m r
        -> EitherContT a l' m r'
bimapEC fl fr ec = EitherContT $ \kl kr -> runEitherContT ec (kl . fl) (kr . fr)

-- |The 'Applicative' instance encodes applicativity of 'EitherContT' 'a' 'l'
-- 'm' 'r' in 'r'.
instance Applicative (EitherContT a l m) where
    pure r = EitherContT $ \_ kr -> kr r
    ecf <*> ec = EitherContT $ \kl kr ->
        runEitherContT ecf kl (\f -> runEitherContT ec kl (kr . f))

-- |'throwEC' encodes the applicative/monadic unit of 'EitherContT' 'a' 'l' 'm'
-- 'r' in 'l'.
throwEC :: l -> EitherContT a l m r
throwEC l = EitherContT $ \kl _ -> kl l

-- |'apL' encodes applicativity of 'EitherContT' 'a' 'l' 'm' 'r' in 'l'.
apL :: EitherContT a (l -> l') m r
    -> EitherContT a l m r
    -> EitherContT a l' m r
apL ecf ec = EitherContT $ \kl kr ->
    runEitherContT ecf (\f -> runEitherContT ec (kl . f) kr) kr

-- |The 'Monad' instance encodes monadicity of 'EitherContT' 'a' 'l' 'm' 'r' in
-- 'r'.
instance Monad (EitherContT a l m) where
    return = pure
    ec >>= ecf = EitherContT $ \kl kr ->
        runEitherContT ec kl (\r -> runEitherContT (ecf r) kl kr)

-- |'throwEC' and 'catchEC' encode monadicity of 'EitherContT' 'a' 'l' 'm' 'r'
-- in 'l'. The usual monad laws hold with 'throwEC' taking the role of 'return'
-- and 'catchEC' taking the role of '>>='.
--
-- prop> throwEC l `catchEC` f = f l
-- prop> ec `catchEC` throwEC = ec
-- prop> (ec `catchEC` f) `catchEC` g = ec `catchEC` (\l -> f l `catchEC` g)
catchEC :: EitherContT a l m r
        -> (l -> EitherContT a l' m r)
        -> EitherContT a l' m r
catchEC ec ecf = EitherContT $ \kl kr ->
    runEitherContT ec (\l -> runEitherContT (ecf l) kl kr) kr

-- |'handleEC' is a flipped `catchEC`.
handleEC :: (l -> EitherContT a l' m r)
         -> EitherContT a l m r
         -> EitherContT a l' m r
handleEC = flip catchEC

-- |A right-to-left, point free way to compose handlers. The monad laws look
-- more elegant, expressed in terms of '<?<'.
--
-- prop> throwEC <?< f = f = f <?< throwEC
-- prop> (h <?< g) <?< f = h <?< (g <?< f)
(<?<) :: (l' -> EitherContT a l'' m r)
      -> (l  -> EitherContT a l'  m r)
      -> (l  -> EitherContT a l'' m r)
(<?<) g f l = g `handleEC` (f l)

-- |A left-to-right, point free way to compose handlers.
(>?>) :: (l  -> EitherContT a l'  m r)
      -> (l' -> EitherContT a l'' m r)
      -> (l  -> EitherContT a l'' m r)
(>?>) = flip (<?<)


-- |'EitherContT' 'a' 'l' 'm' 'r' is a monad transformer for 'm' in 'r'.
instance MonadTrans (EitherContT a l) where
    lift mr = EitherContT $ \_ kr -> mr >>= kr

-- |'EitherContT' 'a' 'l' 'm' 'r' is a monad transformer for 'm' in 'l'.
liftL :: Monad m => m l -> EitherContT a l m r
liftL ml = EitherContT $ \kl _ -> ml >>= kl

-- |'flipEC' encodes the symmetry of 'l' and 'r' in 'EitherContT' 'a' 'l' 'm'
-- 'r'.
--
-- prop> flipEC . flipEC = id
flipEC :: EitherContT a l m r -> EitherContT a r m l
flipEC = EitherContT . flip . runEitherContT

-- |Apply a function to transform the result of a continuation-passing
-- computation.
mapEitherContT :: (m a -> m a) -> EitherContT a l m r -> EitherContT a l m r
mapEitherContT f ec = EitherContT $ \kl kr -> f (runEitherContT ec kl kr)

-- |Apply a function to transform the success continuation passed to a
-- continuation-passing computation.
withEitherContTR :: ((r' -> m a) -> r -> m a)
                 -> EitherContT a l m r
                 -> EitherContT a l m r'
withEitherContTR f ec = EitherContT $ \kl kr -> runEitherContT ec kl (f kr)

-- |Apply a function to transform the failure continuation passed to an
-- continuation-passing computation.
withEitherContTL :: ((l' -> m a) -> l -> m a)
                 -> EitherContT a l m r
                 -> EitherContT a l' m r
withEitherContTL f ec = EitherContT $ \kl kr -> runEitherContT ec (f kl) kr

-- |Call with current success continuation.
instance MonadCont (EitherContT a l m) where
    callCC f = EitherContT $ \kl kr ->
        runEitherContT (f (\r -> (EitherContT $ \_ _ -> kr r))) kl kr

-- |Call with current failure continuation.
callCCL :: ((l -> EitherContT a l' m r) -> EitherContT a l m r)
        -> EitherContT a l m r
callCCL f = EitherContT $ \kl kr ->
    runEitherContT (f (\l -> (EitherContT $ \_ _ -> kl l))) kl kr

-- |The 'MonadError' function 'catchError' is weaker than 'catchEC' since it
-- must not change the error type.
instance MonadError l (EitherContT a l m) where
    throwError = throwEC
    catchError = catchEC

-- |'lowerMonadError' runs the continuation-passing computation, throwing on
-- failure and returning on success.
lowerMonadError :: MonadError l m => EitherContT r l m r -> m r
lowerMonadError ec = runEitherContT ec throwError return

-- |'liftMonadError' embeds a 'MonadError' computation 'm' 'r' into
-- 'EitherContT' 'a' 'l' 'm' 'r'. 
--
-- 'liftMonadError' and 'lowerMonadError' are
-- one-sided inverses, making @MonadError l m => m r@ a retract of
-- 'EitherContT' 'r' 'l' 'm' 'r'.
--
-- prop> lowerMonadError . liftMonadError = id
liftMonadError :: MonadError l m => m r -> EitherContT a l m r
liftMonadError mr = EitherContT $ \kl kr -> (mr >>= kr) `catchError` kl
