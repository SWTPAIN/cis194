fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum 
       . filter even
       . takeWhile (/= 1)
       . iterate (\n -> if even n then n `div` 2 else 3*n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree elem Leaf = Node 0 Leaf elem Leaf
insertTree insertElem (Node depth lTree elem rTree)
  | lTreeHeight < rTreeHeight = Node lTreeHeight insertedLTree elem rTree
  | lTreeHeight > rTreeHeight = Node lTreeHeight lTree elem insertedRTree
  | otherwise                 = Node insertedTreeHeight lTree elem insertedRTree
  where
    lTreeHeight = getTreeHeight lTree
    rTreeHeight = getTreeHeight rTree
    insertedLTree = insertTree insertElem lTree
    insertedRTree = insertTree insertElem rTree
    insertedTreeHeight = getTreeHeight insertedLTree
 
getTreeHeight :: Tree a -> Integer
getTreeHeight Leaf = 0
getTreeHeight (Node height _ _ _) = height

xor :: [Bool] -> Bool
xor = foldr (\x acc -> not x || not acc) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

sieveSundarm :: Integer -> [Integer]
sieveSundarm n = 2 : (doublePlusOne $ [1..n] `exclude` getRemovable halfLimit)
  where halfLimit = (n `div` 2)
        getRemovable :: Integer -> [Integer]
        getRemovable n = map(\(a,b) -> a + b + 2 * a * b) $ cardProd [1..n] [1..n]
        exclude xs ys = filter(\x -> not $ x `elem` ys) xs
        doublePlusOne = map $ (+ 1) . (* 2)


cardProd :: [a] -> [b] -> [(a,b)]
cardProd xs ys = [(x,y) | x <- xs, y <- ys]

