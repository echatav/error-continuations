module Control.Monad.MaybeCont
    ( MaybeCont
    , maybeCont
    , runMaybeCont
    , nothingC
    , mapMaybeCont
    , withMaybeCont
    ) where

import Control.Monad.Trans.MaybeCont
import Data.Functor.Identity

type MaybeCont a r = MaybeContT a Identity r

maybeCont :: (a -> (r -> a) -> a) -> MaybeCont a r
maybeCont mc = MaybeContT $ \ma k -> Identity $
    mc (runIdentity ma) (runIdentity . k)

runMaybeCont :: MaybeCont a r -> (a -> (r -> a) -> a)
runMaybeCont mc a k = runIdentity $
    runMaybeContT mc (Identity a) (Identity . k)

mapMaybeCont :: (a -> a) -> MaybeCont a r -> MaybeCont a r
mapMaybeCont f = mapMaybeContT (Identity . f . runIdentity)

withMaybeCont :: ((r' -> a) -> r -> a)
              -> MaybeCont a r
              -> MaybeCont a r'
withMaybeCont f = withMaybeContT ((Identity .) . f . (runIdentity .))
