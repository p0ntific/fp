import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import qualified Data.Set as Set
import Data.Set (Set)

type Graph = (Int, Int -> Int -> Bool)

isBridge :: Graph -> Int -> Int -> Bool
isBridge (n, adj) u v
    | not (adj u v) = False  -- Если ребра нет, то это не мост
    | otherwise = components < componentsAfterRemoval
    where
        components = countComponents n adj
        -- Создаем новую функцию смежности без данного ребра
        adj' a b
            | (a == u && b == v) || (a == v && b == u) = False
            | otherwise = adj a b
        componentsAfterRemoval = countComponents n adj'

countComponents :: Int -> (Int -> Int -> Bool) -> Int
countComponents n adj = length $ dfsAll n [1..n] adj Set.empty

-- Обход всех вершин и список компонент связности
dfsAll :: Int -> [Int] -> (Int -> Int -> Bool) -> Set Int -> [[Int]]
dfsAll _ [] _ _ = []
dfsAll n (v:vs) adj visited
    | Set.member v visited = dfsAll n vs adj visited
    | otherwise = component : dfsAll n vs adj visited'
    where
        (component, visited') = dfs n v adj visited

-- DFS для одной компоненты
dfs :: Int -> Int -> (Int -> Int -> Bool) -> Set Int -> ([Int], Set Int)
dfs n v adj visited
    | Set.member v visited = ([], visited)
    | otherwise =
        let
            visited' = Set.insert v visited
            neighbors = [u | u <- [1..n], adj v u]
            (nodesList, visited'') = foldl (\(accNodes, accVis) u ->
                let (ns, vis') = dfs n u adj accVis
                in (accNodes ++ ns, vis')) ([], visited') neighbors
        in (v : nodesList, visited'')

-- Используем граф из предыдущего примера
exampleGraph1 :: Graph
exampleGraph1 = (5, adj)
    where
        adj a b = (a, b) `elem` edges || (b, a) `elem` edges
        edges = [(1,2), (1,3), (2,4), (3,5)]

main :: IO ()
main = do
    print $ isBridge exampleGraph1 1 2  -- Должен вывести True
    print $ isBridge exampleGraph1 2 4  -- Должен вывести True