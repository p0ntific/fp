insertHeap :: Ord a => a -> [a] -> [a]
insertHeap x heap = siftUp (heap ++ [x]) (length heap)
  where
    siftUp h 0 = h
    siftUp h i =
      let parent = (i - 1) `div` 2
      in if h !! parent > h !! i
         then siftUp (swap h parent i) parent
         else h
    swap h i j = let hi = h !! i; hj = h !! j
                 in take i h ++ [hj] ++ drop (i + 1) (take j h) ++ [hi] ++ drop (j + 1) h

-- Функция для построения пирамиды из списка элементов
buildHeap :: Ord a => [a] -> [a]
buildHeap = foldl (flip insertHeap) []

-- Создаем список последовательных нечетных чисел
oddNumbers :: [Int]
oddNumbers = [n | n <- [1..], odd n]

-- Первые пять четных чисел
evenNumbers :: [Int]
evenNumbers = [2,4,6,8,10]

-- Построение пирамиды
buildPyramid :: Int -> [Int]
buildPyramid levels = finalHeap  -- Убрали 'take totalElements'
  where
    totalElements = 2 ^ levels - 1  -- Количество узлов в полном бинарном дереве
    initialHeap = buildHeap $ take totalElements oddNumbers
    finalHeap = foldl (flip insertHeap) initialHeap evenNumbers

-- Вывод пирамиды по уровням
printPyramid :: [Int] -> IO ()
printPyramid heap = mapM_ printLevel [0..levels - 1]
  where
    totalNodes = length heap
    levels = ceiling $ logBase 2 (fromIntegral (totalNodes + 1))  -- Используем ceiling вместо floor
    printLevel l = do
      let start = 2^l - 1
      let end = min (2^(l+1) - 1) totalNodes
      print $ take (end - start) $ drop start heap
      
main :: IO ()
main = do
  let levels = 4
  let heap = buildPyramid levels
  putStrLn "Пирамида по уровням:"
  printPyramid heap
-- [1]
-- [2,5]
-- [3,9,11,13]
-- [4,6,10,21,23,25,27,29]
-- [15,7,17,8,19]