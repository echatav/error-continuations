{-# LANGUAGE RankNTypes #-}

module Control.Error.Cont.Util
    ( hush
    , note
    ) where

import Control.Applicative
import Control.Monad.Trans.EitherCont
import Control.Monad.Trans.MaybeCont

hush :: EitherContT a l m r -> MaybeContT a m r
hush ec = MaybeContT $ \ma k -> runEitherContT ec (const ma) k

note :: l -> MaybeContT a m r -> EitherContT a l m r
note l mc = EitherContT $ \kl kr -> runMaybeContT mc (kl l) kr
