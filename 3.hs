-- Функция для вычисления суммы делителей числа n (исключая само число)
sumDivisors :: Int -> Int
sumDivisors n = sum [d | d <- [1 .. n `div` 2], n `mod` d == 0]

-- Функция для поиска первых n пар близнецов
amicablePairs :: Int -> [(Int, Int)]
amicablePairs n = take n [(a, b) | a <- [2..], let b = sumDivisors a, b > a, sumDivisors b == a]
main :: IO ()
main = do
  print $ amicablePairs 3  -- Выведет первые 3 пары близнецов

-- [(220,284),(1184,1210),(2620,2924)]