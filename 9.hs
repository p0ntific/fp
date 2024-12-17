{-# LANGUAGE TupleSections #-}  -- Дополнительно, чтобы разрешить автоматическое создание кортежей
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

type Graph = (Int, Int -> Int -> Bool)
data Color = Red | Blue deriving (Eq, Show)
    
isBipartite :: Graph -> Bool
isBipartite (n, adj) = go [1..n] IntMap.empty
    where
    go [] _ = True
    go (v:vs) colors
        | IntMap.member v colors = go vs colors
        | otherwise = case bfs v colors of
                        Just colors' -> go vs colors'
                        Nothing -> False

    bfs :: Int -> IntMap Color -> Maybe (IntMap Color)
    bfs start colors = bfs' [(start, Red)] colors

    bfs' :: [(Int, Color)] -> IntMap Color -> Maybe (IntMap Color)
    bfs' [] colors = Just colors
    bfs' ((v, c):queue) colors
        | IntMap.member v colors =
            if colors IntMap.! v == c
            then bfs' queue colors
            else Nothing  -- Конфликт цветов
        | otherwise = bfs' (queue ++ neighbors) (IntMap.insert v c colors)
        where
        -- Получаем соседей вершины v
        adjacentVertices = [u | u <- [1..n], adj v u]
        -- Подготовим соседей для обработки, с альтернативным цветом
        neighbors = [(u, oppositeColor c) | u <- adjacentVertices]

    oppositeColor :: Color -> Color
    oppositeColor Red = Blue
    oppositeColor Blue = Red

-- Пример графа, который является двудольным
exampleGraph1 :: Graph
exampleGraph1 = (5, adj)
    where 
    adj a b = (a, b) `elem` edges || (b, a) `elem` edges
    edges = [(1,2), (1,3), (2,4), (3,5)]

-- Пример графа, который не является двудольным (цикл нечетной длины)
exampleGraph2 :: Graph
exampleGraph2 = (3, adj)
    where
    adj a b = (a, b) `elem` edges || (b, a) `elem` edges
    edges = [(1,2), (2,3), (3,1)]

main :: IO ()
main = do
    print $ isBipartite exampleGraph1 
    print $ isBipartite exampleGraph2 

-- True
-- False