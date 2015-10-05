{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.MCMC.Metropolis (mcmc, metropolis) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector (mapM, fromList)
import GHC.Prim (RealWorld)
import Pipes (Producer, yield, (>->), runEffect)
import qualified Pipes.Prelude as Pipes (mapM_, take)
import System.Random.MWC.Probability (Gen, Prob)
import qualified System.Random.MWC.Probability as MWC

-- | A transition operator.
type Transition m = StateT Chain (Prob m) ()

-- | The @Chain@ type specifies the state of a Markov chain at any given
--   iteration.
data Chain = Chain {
    chainTarget   :: Vector Double -> Double
  , chainScore    :: !Double
  , chainPosition :: !(Vector Double)
  }

instance Show Chain where
  show Chain {..} = filter (`notElem` "fromList []") (show chainPosition)

-- | Propose a state transition according to a Gaussian proposal distribution
--   with the specified standard deviation.
propose
  :: PrimMonad m
  => Double
  -> Vector Double
  -> Prob m (Vector Double)
propose radial = Vector.mapM perturb where
  perturb m = MWC.normal m radial

-- | A Metropolis transition operator.
metropolis
  :: PrimMonad m
  => Double
  -> Transition m
metropolis radial = do
  Chain {..} <- get
  proposal <- lift (propose radial chainPosition)
  let proposalScore = chainTarget proposal
      acceptProb    = whenNaN 0 (exp (min 0 (proposalScore - chainScore)))

  accept <- lift (MWC.bernoulli acceptProb)
  when accept (put (Chain chainTarget proposalScore proposal))

-- | A Markov chain.
chain
  :: PrimMonad m
  => Double
  -> Chain
  -> Gen (PrimState m)
  -> Producer Chain m ()
chain radial = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT (metropolis radial) state) prng)
    yield next
    loop next prng

-- | Trace 'n' iterations of a Markov chain.
mcmc
  :: Int
  -> Double
  -> [Double]
  -> (Vector Double -> Double)
  -> Gen RealWorld
  -> IO ()
mcmc n radial (Vector.fromList -> chainPosition) chainTarget gen = runEffect $
        chain radial Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ print
  where
    chainScore = chainTarget chainPosition

-- | Use a provided default value when the argument is NaN.
whenNaN :: RealFloat a => a -> a -> a
whenNaN val x
  | isNaN x   = val
  | otherwise = x
