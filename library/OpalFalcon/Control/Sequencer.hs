{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module OpalFalcon.Control.Sequencer where

import qualified Control.Monad as M
import Data.Function
import Prelude (Int)

data SequenceCase m a
  = Break a
  | BreakM (m a)
  | Continue (m (SequenceCase m a))
  | Recurse (m (SequenceCase m a)) (a -> m (SequenceCase m a))
  | Repeat (m (SequenceCase m a)) Int ([a] -> m (SequenceCase m a))

-- Sequences a monadic operation in another monad
sequence :: (M.Monad m0, M.Monad m1) => (forall b. (m0 b -> m1 b)) -> m0 (SequenceCase m0 a) -> m1 a
sequence eval step =
  do
    x <- eval step
    case x of
      Break val -> M.return val
      BreakM val0 -> eval val0
      Continue next -> do
        sequence eval next
      Recurse recOp next -> do
        recResult <- sequence eval recOp
        sequence eval $ next recResult
      Repeat f cnt cont -> do
        vals <- M.replicateM cnt (sequence eval f)
        sequence eval $ cont vals

forceSpine :: (M.Monad m) => m (SequenceCase m a) -> m a
forceSpine spine = sequence id spine

-- data LiftedMonad m0 m1 a = LM (forall b. (m0 b -> m1 b)) (m0 a)
-- 
-- bindLM :: forall a b. (Monad m0, Monad m1) => LiftedMonad m0 m1 a -> (a -> LiftedMonad m0 m1 b) -> LiftedMonad m0 m1 b
-- bindLM (LM eval val) f = 

