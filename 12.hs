isPrimeSimple :: Int -> Bool
isPrimeSimple n
  | n <= 1 = False
  | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]
  where
    isqrt = floor . sqrt . fromIntegral

primesOfForm :: [Int]
primesOfForm = filter isPrimeSimple numbers


numbers :: [Int]
numbers = [2 * n + 1 | n <- [1..]]

main :: IO ()
main = do
  let firstPrimes = take 10 primesOfForm  
  print firstPrimes
-- [3,5,7,11,13,17,19,23,29,31]
