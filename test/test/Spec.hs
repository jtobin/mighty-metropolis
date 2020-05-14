import Test.Hspec
import Data.Sampling.Types
import Data.Maybe (fromJust)
import Numeric.MCMC.Metropolis (chain,chain')
import System.Random.MWC

withinPercent :: Double -> Double -> Double -> Bool
withinPercent a 0 _ = a == 0
withinPercent a b n = d / b < (n / 100)
  where
    d = abs (a - b)

mean :: [Double] -> Double
mean xs = sum xs / n
  where
    n = fromIntegral (length xs)

variance :: [Double] -> Double
variance xs = sum [(x - m) ** 2.0 | x <- xs] / (n - 1)
  where
    m = mean xs
    n = fromIntegral (length xs)

stdDev :: [Double] -> Double
stdDev = sqrt . variance

stdErr :: [Double] -> Double
stdErr xs = stdDev xs / sqrt n
  where
    n = fromIntegral (length xs)


testHelperFunctions :: SpecWith ()
testHelperFunctions = describe "Testing helper functions" $ do
  it "test withinPercent works" $ do
    withinPercent 106 100 5 `shouldBe` False
    withinPercent 105 100 5 `shouldBe` False
    withinPercent 104 100 5 `shouldBe` True
    withinPercent 96 100 5 `shouldBe` True
    withinPercent 95 100 5 `shouldBe` False
    withinPercent 94 100 5 `shouldBe` False
  it "test mean works" $ do
    withinPercent (mean [1,2,3]) 2 1e-3 `shouldBe` True
    withinPercent (mean [1..100]) 50.5 1e-3 `shouldBe` True
    withinPercent (mean [1..1000000]) 500000.5 1e-3 `shouldBe` True
  it "test variance works" $ do
    withinPercent (variance [0,1]) 0.5 1e-3 `shouldBe` True
    withinPercent (variance [1,1,1]) 0 1e-3 `shouldBe` True
    withinPercent (variance [1..100]) 841.66666666 1e-3 `shouldBe` True
  it "test stdErr works" $ do
    withinPercent (stdErr [1..100]) 2.901149 1e-3 `shouldBe` True
    withinPercent (stdErr [1..1000]) 9.133273 1e-3 `shouldBe` True

thin :: Int -> [a] -> [a]
thin _ [] = []
thin n (x:xs) = x : thin n (drop (n - 1) xs)

getChainResults :: IO [Double]
getChainResults =
  let numIters = 1000000
      radialSize = 0.2
      x0 = [1.0]
      lnObj [x] =
        if x > 0
          then -x
          else -1 / 0
      thinning = 1000
   in do boxedXs <-
           withSystemRandom . asGenIO $ chain numIters radialSize x0 lnObj
         return $ thin thinning $ head . chainPosition <$> boxedXs

testChainResults :: [Double] -> SpecWith ()
testChainResults xs =
  describe "Testing samples from exponential distribution with rate 1" $ do
    it "test mean is estimated correctly" $ do
      mean xs < 1 + 2 * stdErr xs `shouldBe` True
      mean xs > 1 - 2 * stdErr xs `shouldBe` True
    it "test variance is estimated correctly" $ do
      variance xs < 1 + 2 * stdErr [(x - 1) ** 2.0 | x <- xs] `shouldBe` True
      variance xs > 1 - 2 * stdErr [(x - 1) ** 2.0 | x <- xs] `shouldBe` True

getTunableResults :: IO [Double]
getTunableResults =
  let numIters = 1000000
      radialSize = 0.2
      x0 = [1.0]
      lnObj [x] =
        if x > 0
          then -x
          else -1 / 0
      tunable [x] = x ** 3.0
      thinning = 1000
   in do boxedXs <-
           withSystemRandom . asGenIO $ chain' numIters radialSize x0 lnObj (Just tunable)
         return $ thin thinning $ fromJust . chainTunables <$> boxedXs

testTunableResults :: [Double] -> SpecWith ()
testTunableResults ts =
  describe "Testing third moment of exponential distribution with rate 1" $ do
    it "test third moment (which is 6) is estimated correctly" $ do
      mean ts < 6 + 2 * stdErr ts `shouldBe` True
      mean ts > 6 - 2 * stdErr ts `shouldBe` True

main :: IO ()
main = do
  xs <- getChainResults
  ts <- getTunableResults
  hspec $ do
    testHelperFunctions
    testChainResults xs
    testTunableResults ts
