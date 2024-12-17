-- Функция для разбиения списка на возрастающие подпоследовательности
splitIncreasing :: Ord a => [a] -> [[a]]
splitIncreasing [] = []
splitIncreasing (x:xs) = go [x] xs
  where
    go acc [] = [reverse acc]
    go acc (y:ys)
      | y > head acc = go (y:acc) ys
      | otherwise = reverse acc : splitIncreasing (y:ys)

main :: IO ()
main = do
  let nums = [2,7,10,8,3,4,9,1,2,0,8,3,2,5]
  let result = splitIncreasing nums
  print result 
 -- Выведет [[2,7,10],[8],[3,4,9],[1,2],[0,8],[3],[2,5]]