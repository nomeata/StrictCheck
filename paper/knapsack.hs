import Data.Map.Lazy

main = return ()

ws :: [Int]
ws = [10, 20, 30]

vs :: [Int]
vs = [60, 100, 120]

w :: Int
w = 50

solution :: Map (Int, Int) Int
solution = fromList [ ((j, k),
                       if (j-1) < 0 || k - ws !! j < 0
                       then 0
                       else max (solution ! (j-1, k))
                                (solution ! (j-1, k - ws !! j) + vs !! j)
                      )
                    | j <- [0..2], k <- [0..50] ]
