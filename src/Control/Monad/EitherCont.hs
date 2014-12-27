module Control.Monad.EitherCont
    ( EitherCont
    , eitherCont
    , runEitherCont
    , eitherEC
    , fmapL
    , bimapEitherC
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

type EitherCont a l r = EitherContT a l Identity r

eitherCont :: ((l -> a) -> (r -> a) -> a) -> EitherCont a l r
eitherCont ec = EitherContT $ \kl kr -> Identity $
    ec (runIdentity . kl) (runIdentity . kr)

runEitherCont :: EitherCont a l r -> (l -> a) -> (r -> a) -> a
runEitherCont ec kl kr = runIdentity $
    runEitherContT ec (Identity . kl) (Identity . kr)

eitherEC :: Either l r -> EitherCont a l r
eitherEC e = eitherCont $ \kl kr -> either kl kr e

mapEitherCont :: (a -> a) -> EitherCont a l r -> EitherCont a l r
mapEitherCont f = mapEitherContT (Identity . f . runIdentity)

withEitherContR :: ((r' -> a) -> r -> a)
                -> EitherCont a l r
                -> EitherCont a l r'
withEitherContR f = withEitherContTR ((Identity .) . f . (runIdentity .))

withEitherContL :: ((l' -> a) -> l -> a)
                -> EitherCont a l r
                -> EitherCont a l' r
withEitherContL f = withEitherContTL ((Identity .) . f . (runIdentity .))
