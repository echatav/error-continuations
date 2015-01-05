{-|
Module      : Control.Error.Cont.Util
Description : Utility functions for error continuations
Copyright   : (c) Eitan Chatav, 2015
License     : PublicDomain
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental

This module provides utility functions to convert between 'MaybeContT' and
'EitherContT' computations.
-}

module Control.Error.Cont.Util
    ( hush
    , note
    ) where

import Control.Applicative
import Control.Monad.Trans.EitherCont
import Control.Monad.Trans.MaybeCont

-- |Suppress the nothing continuation of an 'EitherContT'
hush :: EitherContT a l m r -> MaybeContT a m r
hush ec = MaybeContT $ \ma k -> runEitherContT ec (const ma) k

-- |Tag the nothing continuation of a 'MaybeContT'
note :: l -> MaybeContT a m r -> EitherContT a l m r
note l mc = EitherContT $ \kl kr -> runMaybeContT mc (kl l) kr
