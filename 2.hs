-- Функция для получения списка делителей числа n (исключая само число)
divisors :: Int -> [Int]
divisors n = [d | d <- [1 .. n `div` 2], n `mod` d == 0]
   
-- Функция для проверки, является ли число совершенным
isPerfect :: Int -> Bool
isPerfect n = sum (divisors n) == n

-- Функция для нахождения первых n совершенных чисел
perfectNumbers :: Int -> [Int]
perfectNumbers n = take n [x | x <- [2..], isPerfect x]

main :: IO ()
main = do
  print $ divisors 5
  print $ isPerfect 5
  print $ perfectNumbers 4


-- [1]
-- False
-- [6,28,496,8128]