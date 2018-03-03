import Data.Map.Lazy hiding (foldr)
import Data.Function

main = return ()

weights :: [Int]
weights = [10, 20, 30]

values :: [Int]
values = [60, 100, 120]

limit :: Int
limit = 50

solutionStep :: Map (Int, Int) Int -> Map (Int, Int) Int
solutionStep soln =
  fromList [((j, k), knapsack j k) | j <- [0 .. length weights-1], k <- [0 .. limit]]
  where
    knapsack j k = if j - 1 < 0 || k - weights !! j < 0
                   then if weights !! j <= k then values !! j else 0
                   else max (soln ! (j-1, k))
                            (soln ! (j-1, k - weights !! j) + values !! j)

solution :: Map (Int, Int) Int
solution = fix solutionStep
