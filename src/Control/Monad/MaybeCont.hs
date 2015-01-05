{-|
Module      : Control.Monad.MaybeContT
Description : The 'MaybeCont' type and API
Copyright   : (c) Eitan Chatav, 2015
License     : PublicDomain
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental

The 'MaybeCont' type and API provide an idiomatic way to handle possibly
failing computations in continuation passing style.
-}

module Control.Monad.MaybeCont
    ( MaybeCont
    , MaybeContT()
    , maybeCont
    , runMaybeCont
    , liftMaybe
    , nothingC
    , mapMaybeCont
    , withMaybeContJust
    , withMaybeContNothing
    ) where

import Control.Monad.Trans.MaybeCont
import Data.Functor.Identity

-- |'MaybeCont' 'a' 'r' is a CPS computation that produces an intermediate
-- result of type 'a' within a CPS computation which produces either a just
-- or nothing.
type MaybeCont a r = MaybeContT a Identity r

-- |Construct a continuation-passing computation from a function.
maybeCont :: (a -> (r -> a) -> a) -> MaybeCont a r
maybeCont mc = MaybeContT $ \ma k -> Identity $
    mc (runIdentity ma) (runIdentity . k)

-- |The result of running a CPS computation with given nothing and just
-- continuations.
--
-- prop> runMaybeCont . maybeCont = id
-- prop> maybeCont . runMaybeCont = id
runMaybeCont :: MaybeCont a r -> (a -> (r -> a) -> a)
runMaybeCont mc a k = runIdentity $
    runMaybeContT mc (Identity a) (Identity . k)

-- |'liftMaybe' embeds 'Maybe' in 'MaybeCont' 'a'.
liftMaybe :: Maybe r -> MaybeCont a r
liftMaybe may = maybeCont $ \a k -> maybe a k may

-- |Apply a function to transform the result of a continuation-passing
-- computation.
mapMaybeCont :: (a -> a) -> MaybeCont a r -> MaybeCont a r
mapMaybeCont = mapMaybeContT . fmap

-- |Apply a function to transform the just continuation passed to a
-- continuation-passing computation.
withMaybeContJust :: ((r' -> a) -> r -> a)
               -> MaybeCont a r
               -> MaybeCont a r'
withMaybeContJust f = withMaybeContTJust ((Identity .) . f . (runIdentity .))

-- |Apply a function to transform the nothing continuation passed to an
-- continuation-passing computation.
withMaybeContNothing :: (a -> a) -> MaybeCont a r -> MaybeCont a r
withMaybeContNothing = withMaybeContTNothing . fmap
