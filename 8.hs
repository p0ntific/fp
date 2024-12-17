import Data.List (unfoldr)

-- Вычисляет факториал n
factorial :: Integer -> Integer
factorial n = product [1 .. n]

-- Генерирует члены ряда
seriesTerms :: Double -> [Double]
seriesTerms x = [ x ** fromIntegral n / (fromIntegral (factorial n) + 1) | n <- [0..] ]

-- Суммирует ряд до тех пор, пока член больше ε по абсолютной величине
sumSeries :: Double -> Double -> Double
sumSeries x epsilon = sumUntil (seriesTerms x) epsilon 0.0
  where
    sumUntil :: [Double] -> Double -> Double -> Double
    sumUntil [] _ acc = acc
    sumUntil (t:ts) e acc
      | abs t < e = acc
      | otherwise = sumUntil ts e (acc + t)

main :: IO ()
main = do
  let x = 2.0       -- значение x
  let epsilon = 0.001  -- точность
  let sumResult = sumSeries x epsilon
  print sumResult
-- 5.002570656519159