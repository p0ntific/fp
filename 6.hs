import qualified Data.Char as Char

-- Проверяет, содержит ли строка хотя бы одну цифру
containsDigit :: String -> Bool
containsDigit = any Char.isDigit

-- Фильтрует строки, исключая содержащие цифры
filterStrings :: [String] -> [String]
filterStrings = filter (not . containsDigit)

main :: IO ()
main = do
  let strings = ["hello", "world123", "test", "example1", "Haskell"]
  let result = filterStrings strings
  print result  
-- ["hello","test","Haskell"]