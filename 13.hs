terms :: [Double]
terms = [1 / fromIntegral (factorial n) | n <- [0..]]
    where
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

partialSums :: [Double]
partialSums = scanl1 (+) terms
main :: IO ()
main = do
  print $ take 10 partialSums

-- [1.0,2.0,2.5,2.6666666666666665,2.708333333333333,2.7166666666666663,2.7180555555555554,2.7182539682539684,2.71827876984127,2.7182815255731922]