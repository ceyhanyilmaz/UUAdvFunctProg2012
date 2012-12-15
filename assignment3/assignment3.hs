module Assignment(Dictionary(Dict), create_dictionary, lookup,
  update, fold, rebalance, keys, samekeys,
  papercuts, permutations, genwords) where

data Tree a b = Empty | Node a b (Tree a b) (Tree a b)
  deriving (Show)

data Dictionary a b = Dict (Tree a b) (a -> a -> Ordering) b

create_dictionary :: (a -> a -> Ordering) -> b -> Dictionary a b
create_dictionary cmp def = Dict Empty cmp def

lookup_aux :: (Tree a b) -> a -> (a -> a -> Ordering) -> b -> b
lookup_aux Empty _ _ def = def
lookup_aux (Node k v l r) key cmp def
  | cmp key k == EQ = v
  | cmp key k == LT = lookup_aux l key cmp def
  | cmp key k == GT = lookup_aux r key cmp def

loookup :: Dictionary a b -> a -> b
loookup (Dict root cmp def) key = lookup_aux root key cmp def

update_aux :: a -> b -> Tree a b -> (a -> a -> Ordering) -> Tree a b
update_aux key value Empty _ = Node key value Empty Empty
update_aux key value (Node k v l r) cmp
  | cmp key k == EQ = Node k value l r
  | cmp key k == LT = Node k v (update_aux key value l cmp) r
  | cmp key k == GT = Node k v l (update_aux key value r cmp)

update :: a -> b -> Dictionary a b -> Dictionary a b
update key value (Dict root cmp def) =
  Dict (update_aux key value root cmp) cmp def

fold_aux :: (t -> t1 -> t2 -> t2) -> Tree t t1 -> t2 -> t2
fold_aux fun Empty initial = initial
fold_aux fun (Node k v l r) initial = fold_aux fun l
                                       (fun k v
                                         (fold_aux fun r initial))

fold :: (t -> t1 -> t2 -> t2) -> Dictionary t t1 -> t2 -> t2
fold fun (Dict root _ _) initial = fold_aux fun root initial

rebalance_aux :: [(a,b)] -> Tree a b
rebalance_aux [] = Empty
rebalance_aux l = 
  let (left,(key,value):right) = splitAt (div (length l) 2) l
  in Node key value (rebalance_aux left) (rebalance_aux right)

rebalance :: Dictionary a b -> Dictionary a b
rebalance d@(Dict root cmp def) =
  Dict (rebalance_aux (fold (\k v acc -> (k,v):acc) d [])) cmp def

keys :: Dictionary a t -> [a]
keys d = fold (\k v acc -> k:acc) d []

list_equals :: [t] -> [t1] -> (t -> t1 -> Ordering) -> Ordering
list_equals [] [] _ = EQ
list_equals _ [] _ = GT
list_equals [] _ _ = LT
list_equals (h1:t1) (h2:t2) cmp
  | cmp h1 h2 == EQ = list_equals t1 t2 cmp
  | otherwise = cmp h1 h2

samekeys :: Eq a => Dictionary a t -> Dictionary a t1 -> Ordering
samekeys d1@(Dict _ cmp _) d2 = list_equals (keys d1) (keys d2) cmp


papercuts_aux :: [Char] -> [Char] -> [([Char], [Char])]
papercuts_aux s1 "" = [(s1,"")]
papercuts_aux s1 s2@(h:t) = (s1,s2):(papercuts_aux (s1++[h]) t)

papercuts :: [Char] -> [([Char], [Char])]
papercuts s = papercuts_aux "" s


delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t)
  | x == h = t
  | otherwise = h:(delete x t)

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations l = [h:t | h <- l, t <- (permutations (delete h l))]

genwords_aux :: [a] -> [[a]]
genwords_aux l = [h:t | t <- []:(genwords_aux l), h <- l]

genwords :: [a] -> [[a]]
genwords l = []:genwords_aux l
