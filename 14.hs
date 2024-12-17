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
evenNumbers = take 5 [n | n <- [2..], even n]

-- Построение пирамиды
buildPyramid :: Int -> [Int]
buildPyramid levels = take totalElements finalHeap
  where
    totalElements = 2 ^ levels - 1  -- Количество узлов в полном бинарном дереве
    initialHeap = buildHeap $ take totalElements oddNumbers
    finalHeap = foldl (flip insertHeap) initialHeap evenNumbers

-- Вывод пирамиды по уровням
printPyramid :: [Int] -> IO ()
printPyramid heap = mapM_ printLevel [0..levels - 1]
  where
    levels = floor $ logBase 2 (fromIntegral (length heap + 1))
    printLevel l = print $ take (2^l) $ drop (2^l - 1) heap

main :: IO ()
main = do
  let levels = 4
  let heap = buildPyramid levels
  putStrLn "Пирамида по уровням:"
  printPyramid heap


-- Пирамида по уровням:
-- [1]
-- [2,5]
-- [3,9,11,13]
-- [4,6,10,21,23,25,27,29]