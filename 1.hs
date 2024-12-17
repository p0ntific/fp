-- Функция для вычисления n-го члена последовательности
seqA :: Int -> Int
seqA n
  | n == 0 = 1
  | n == 1 = 2
  | otherwise = sequenceIter n 1 2 2
  where
    sequenceIter :: Int -> Int -> Int -> Int -> Int
    sequenceIter n a0 a1 i
      | i > n = a1
      | otherwise = sequenceIter n a1 (3 * a1 - 2 * a0 + 1) (i + 1)

main :: IO ()
main = do
  print $ seqA 5  -- Вычисляет 5-й член последовательности

-- 58