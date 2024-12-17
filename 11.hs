import Data.List (nub, sort, union)

-- Бесконечные списки квадратов, кубов и факториалов
squares :: [Integer]
squares = [n^2 | n <- [1..]]

cubes :: [Integer]
cubes = [n^3 | n <- [1..]]

factorials :: [Integer]
factorials = scanl1 (*) [1..]

-- Функция для слияния списков без повторений и сортировки
mergeLists :: [Integer] -> [Integer] -> [Integer] -> [Integer]
mergeLists xs ys zs = mergeThree xs ys zs

-- Функция для слияния трех упорядоченных списков без повторений
mergeThree :: [Integer] -> [Integer] -> [Integer] -> [Integer]
mergeThree xs ys zs = merge xs (merge ys zs)
  where
    merge :: [Integer] -> [Integer] -> [Integer]
    merge [] bs = bs
    merge as [] = as
    merge (a:as) (b:bs)
      | a < b = a : merge as (b:bs)
      | a > b = b : merge (a:as) bs
      | otherwise = a : merge as bs  -- Пропускаем повторения

-- Генерируем объединенную последовательность
mySequenceA :: [Integer]
mySequenceA = mergeThree squares cubes factorials

-- Функция для получения n-го элемента
nthElement :: Int -> Integer
nthElement n = mySequenceA !! (n - 1)  -- Индексация с нуля
main :: IO ()
main = do
  let n = 10
  print $ take n mySequenceA       
  print $ nthElement n

-- [1,2,4,6,8,9,16,24,25,27]
-- 27