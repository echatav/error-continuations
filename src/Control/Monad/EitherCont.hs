{-|
Module      : Control.Monad.EitherCont
Description : The 'EitherCont' type and API
Copyright   : (c) Eitan Chatav, 2015
License     : PublicDomain
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental

The 'EitherCont' type and API provide an idiomatic way to handle errors in
continuation passing style.
-}

module Control.Monad.EitherCont
    ( EitherCont
    , EitherContT()
    , eitherCont
    , runEitherCont
    , liftEither
    , fmapL
    , bimapEC
    , throwEC
    , apL
    , catchEC
    , liftL
    , flipEC
    , mapEitherCont
    , withEitherContL
    , withEitherContR
    , callCCL
    ) where

import Control.Monad.Trans.EitherCont
import Data.Functor.Identity

-- |'EitherCont' 'a' 'l' 'r' is a CPS computation that produces an intermediate
-- result of type 'a' within a CPS computation which produces either a success
-- of type 'r' or failure of type 'l'.
type EitherCont a l r = EitherContT a l Identity r

-- |Construct a continuation-passing computation from a function.
eitherCont :: ((l -> a) -> (r -> a) -> a) -> EitherCont a l r
eitherCont ec = EitherContT $ \kl kr -> Identity $
    ec (runIdentity . kl) (runIdentity . kr)

-- |The result of running a CPS computation with given failure and success
-- continuations.
--
-- prop> runEitherCont . eitherCont = id
-- prop> eitherCont . runEitherCont = id
runEitherCont :: EitherCont a l r -> (l -> a) -> (r -> a) -> a
runEitherCont ec kl kr = runIdentity $
    runEitherContT ec (Identity . kl) (Identity . kr)

-- |'liftEither' embeds 'Either' in 'EitherCont' 'a'.
liftEither :: Either l r -> EitherCont a l r
liftEither e = eitherCont $ \kl kr -> either kl kr e

-- |Apply a function to transform the result of a continuation-passing
-- computation.
mapEitherCont :: (a -> a) -> EitherCont a l r -> EitherCont a l r
mapEitherCont = mapEitherContT . fmap

-- |Apply a function to transform the success continuation passed to a
-- continuation-passing computation.
withEitherContR :: ((r' -> a) -> r -> a)
                -> EitherCont a l r
                -> EitherCont a l r'
withEitherContR f = withEitherContTR ((Identity .) . f . (runIdentity .))

-- |Apply a function to transform the failure continuation passed to an
-- continuation-passing computation.
withEitherContL :: ((l' -> a) -> l -> a)
                -> EitherCont a l r
                -> EitherCont a l' r
withEitherContL f = withEitherContTL ((Identity .) . f . (runIdentity .))
