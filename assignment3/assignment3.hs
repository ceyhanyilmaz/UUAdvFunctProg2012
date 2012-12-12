data Tree a b = Empty | Node a b (Tree a b) (Tree a b)
  deriving (Show)

data Dictionary a b = Dict (a -> a -> Ordering) (Tree a b)

test :: Tree Int Int
test = Node 4 4 (Node 3 3 Empty Empty) Empty

test2 :: Dictionary Int Int
test2 = Dict compare test
