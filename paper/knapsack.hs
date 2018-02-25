import Data.Map.Lazy
import Data.Function

main = return ()

ws :: [Int]
ws = [10, 20, 30]

vs :: [Int]
vs = [60, 100, 120]

w :: Int
w = 50

solutionPre :: Map (Int, Int) Int -> Map (Int, Int) Int
solutionPre soln = fromList [ ((j, k),
                               if (j-1) < 0 || k - ws !! j < 0
                               then if ws !! j <= k then vs !! j else 0
                               else max (soln ! (j-1, k))
                                        (soln ! (j-1, k - ws !! j) + vs !! j)
                              )
                            | j <- [0..2], k <- [0..50] ]

solution :: Map (Int, Int) Int
solution = fix solutionPre
