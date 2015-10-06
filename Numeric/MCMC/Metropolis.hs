{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.MCMC.Metropolis (mcmc, metropolis) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import GHC.Prim (RealWorld)
import Pipes (Producer, yield, (>->), runEffect)
import qualified Pipes.Prelude as Pipes (mapM_, take)
import System.Random.MWC.Probability (Gen, Prob)
import qualified System.Random.MWC.Probability as MWC

-- | A transition operator.
type Transition m a = StateT a (Prob m) ()

-- | The @Chain@ type specifies the state of a Markov chain at any given
--   iteration.
data Chain a b = Chain {
    chainTarget   :: a -> Double
  , chainScore    :: !Double
  , chainPosition :: a
  , chainTunables :: Maybe b
  }

instance Show a => Show (Chain a b) where
  show Chain {..} = filter (`notElem` "fromList []") (show chainPosition)

-- | Propose a state transition according to a Gaussian proposal distribution
--   with the specified standard deviation.
propose
  :: (PrimMonad m, Traversable f)
  => Double
  -> f Double
  -> Prob m (f Double)
propose radial = traverse perturb where
  perturb m = MWC.normal m radial

-- | A Metropolis transition operator.
metropolis
  :: (Traversable f, PrimMonad m)
  => Double
  -> Transition m (Chain (f Double) b)
metropolis radial = do
  Chain {..} <- get
  proposal <- lift (propose radial chainPosition)
  let proposalScore = chainTarget proposal
      acceptProb    = whenNaN 0 (exp (min 0 (proposalScore - chainScore)))

  accept <- lift (MWC.bernoulli acceptProb)
  when accept (put (Chain chainTarget proposalScore proposal chainTunables))

-- | A Markov chain.
chain
  :: (Traversable f, PrimMonad m)
  => Double
  -> Chain (f Double) b
  -> Gen (PrimState m)
  -> Producer (Chain (f Double) b) m ()
chain radial = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (metropolis radial) state) prng)
    yield next
    loop next prng

-- | Trace 'n' iterations of a Markov chain.
mcmc
  :: (Traversable f, Show (f Double))
  => Int
  -> Double
  -> f Double
  -> (f Double -> Double)
  -> Gen RealWorld
  -> IO ()
mcmc n radial chainPosition chainTarget gen = runEffect $
        chain radial Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ print
  where
    chainScore    = chainTarget chainPosition
    chainTunables = Nothing

-- | Use a provided default value when the argument is NaN.
whenNaN :: RealFloat a => a -> a -> a
whenNaN val x
  | isNaN x   = val
  | otherwise = x
