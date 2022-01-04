insert :: Ord a => a -> [a] -> [a]
insert e ls = insertBy (compare) e ls

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy cmp x ys@(y:ys')
  = case cmp x y of
    GT -> y : insertBy cmp x ys'
    _  -> x : ys

sort :: Ord a => [a] -> [a]
sort = foldr insert []

main :: IO()
main = do
  let x = sort [1, 2, 3, 4, 1]
  print x
