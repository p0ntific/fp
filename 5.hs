-- Выбирает элементы списка с нечетными индексами (начинаем с 1)
oddIndexedElements :: [a] -> [a]
oddIndexedElements xs = [x | (i, x) <- zip [1..] xs, odd i]

-- Удаляет каждый второй символ из строки
removeEverySecondChar :: String -> String
removeEverySecondChar s = [c | (i, c) <- zip [1..] s, odd i]

-- Основная функция обработки списка строк
processStrings :: [String] -> [String]
processStrings xs = map removeEverySecondChar (oddIndexedElements xs)

main :: IO ()
main = do
  let input = ["couple", "of", "two", "natural", "numbers", "are", "twins"]
  let result = processStrings input
  print result 
-- ["cul","to","nmes","tis"]