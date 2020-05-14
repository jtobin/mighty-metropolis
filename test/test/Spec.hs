import Test.Hspec

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

main :: IO ()
main = hspec $ do
  testHelperFunctions
