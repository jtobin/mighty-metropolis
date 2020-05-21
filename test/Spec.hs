{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Control.Foldl as L
import Data.Functor.Identity
import Data.Maybe (mapMaybe)
import Data.Sampling.Types
import Numeric.MCMC.Metropolis (chain, chain')
import System.Random.MWC
import Test.Hspec

withinPercent :: Double -> Double -> Double -> Bool
withinPercent b n a
    | b == 0    = a == 0
    | otherwise = d / b < n / 100
  where
    d = abs (a - b)

mean :: [Double] -> Double
mean = L.fold L.mean

variance :: [Double] -> Double
variance xs = L.fold alg xs where
  alg = (/) <$> L.premap csq L.sum <*> L.genericLength - 1
  csq = (** 2.0) . subtract m
  m   = mean xs

stdDev :: [Double] -> Double
stdDev = sqrt . variance

stdErr :: [Double] -> Double
stdErr xs = stdDev xs / sqrt n where
  n = fromIntegral (length xs)

thin :: Int -> [a] -> [a]
thin n xs = case xs of
  (h:t) -> h : thin n (drop (pred n) t)
  _     -> mempty

data Params = Params {
    pepochs  :: Int
  , pradial  :: Double
  , porigin  :: Identity Double
  , ptunable :: Maybe (Identity Double -> Double)
  , pltarget :: Identity Double -> Double
  , pthin    :: Int
  }

testParams :: Params
testParams = Params {
    pepochs  = 1000000
  , pradial  = 0.2
  , porigin  = Identity 1.0
  , ptunable = Just (\(Identity x) -> x ** 3.0)
  , pltarget = \(Identity x) -> if x > 0 then negate x else negate 1 / 0
  , pthin    = 1000
  }

getChainResults :: IO [Double]
getChainResults = do
  let Params {..} = testParams

  boxed <- withSystemRandom . asGenIO $
    chain pepochs pradial porigin pltarget

  let positions = fmap (runIdentity . chainPosition) boxed
  pure (thin pthin positions)

getTunableResults :: IO [Double]
getTunableResults = do
  let Params {..} = testParams

  boxed <- withSystemRandom . asGenIO $
    chain' pepochs pradial porigin pltarget ptunable

  let positions = mapMaybe chainTunables boxed
  pure (thin pthin positions)

testWithinPercent :: SpecWith ()
testWithinPercent = describe "withinPercent" $
  it "works as expected" $ do
    106 `shouldNotSatisfy` withinPercent 100 5
    105 `shouldNotSatisfy` withinPercent 100 5
    104 `shouldSatisfy`    withinPercent 100 5
    96  `shouldSatisfy`    withinPercent 100 5
    95  `shouldNotSatisfy` withinPercent 100 5
    94  `shouldNotSatisfy` withinPercent 100 5

testMean :: SpecWith ()
testMean = describe "mean" $
  it "works as expected" $ do
    mean [1, 2, 3]    `shouldSatisfy` withinPercent 2 1e-3
    mean [1..100]     `shouldSatisfy` withinPercent 50.5 1e-3
    mean [1..1000000] `shouldSatisfy` withinPercent 500000.5 1e-3

testVariance :: SpecWith ()
testVariance = describe "variance" $
  it "works as expected" $ do
    variance [0, 1]    `shouldSatisfy` withinPercent 0.5 1e-3
    variance [1, 1, 1] `shouldSatisfy` withinPercent 0 1e-3
    variance [1..100]  `shouldSatisfy` withinPercent 841.66666666 1e-3

testStdErr :: SpecWith ()
testStdErr = describe "stdErr" $
  it "works as expected" $ do
    stdErr [1..100]  `shouldSatisfy` withinPercent 2.901149 1e-3
    stdErr [1..1000] `shouldSatisfy` withinPercent 9.133273 1e-3

testHelperFunctions :: SpecWith ()
testHelperFunctions = describe "helper functions" $ do
  testWithinPercent
  testMean
  testVariance
  testStdErr

testSamples :: [Double] -> SpecWith ()
testSamples xs = describe "sampled trace over exp(1)" $ do
  let meanStdErr = stdErr xs
      varStdErr  = stdErr (fmap (\x -> pred x ** 2.0) xs)

  context "within three standard errors" $ do
    it "has the expected mean" $ do
      mean xs `shouldSatisfy` (< 1 + 3 * meanStdErr)
      mean xs `shouldSatisfy` (> 1 - 3 * meanStdErr)

    it "has the expected variance" $ do
      variance xs `shouldSatisfy` (< 1 + 3 * varStdErr)
      variance xs `shouldSatisfy` (> 1 - 3 * varStdErr)

testTunables :: [Double] -> SpecWith ()
testTunables ts = describe "sampled tunables over exp(1)" $ do
  let meanStdErr = stdErr ts

  context "within three standard errors" $
    it "has the expected third moment" $ do
      mean ts `shouldSatisfy` (< 6 + 3 * meanStdErr)
      mean ts `shouldSatisfy` (> 6 - 3 * meanStdErr)

main :: IO ()
main = do
  xs <- getChainResults
  ts <- getTunableResults

  hspec $ do
    testHelperFunctions
    testSamples xs
    testTunables ts
