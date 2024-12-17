import Data.List (maximumBy)
import Data.Function (on)

-- Функция для поиска самой длинной строки
longestString :: [String] -> String
longestString = maximumBy (compare `on` length)

main :: IO ()
main = do
  let strings = ["apple", "banan", "money", "beautiful"]
  print $ longestString strings
  
-- beautiful