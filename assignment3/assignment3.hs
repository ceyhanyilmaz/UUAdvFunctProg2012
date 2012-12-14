data Tree a b = Empty | Node a b (Tree a b) (Tree a b)
  deriving (Show)

data Dictionary a b = Dict (Tree a b) (a -> a -> Ordering) b

test :: Tree Int Int
test = Node 4 4 (Node 3 3 Empty Empty) Empty

test2 :: Dictionary Int Int
test2 = Dict test compare 0

create_dictionary :: (a -> a -> Ordering) -> b -> Dictionary a b
create_dictionary cmp def = Dict Empty cmp def

test3 :: Dictionary Int Int
test3 = create_dictionary compare 0

lookup_aux :: (Tree a b) -> a -> (a -> a -> Ordering) -> b -> b
lookup_aux Empty _ _ def = def
lookup_aux (Node k v l r) key cmp def
  | cmp key k == EQ = v
  | cmp key k == LT = lookup_aux l key cmp def
  | cmp key k == GT = lookup_aux r key cmp def

test4 = lookup_aux test 4 compare 1
test5 = lookup_aux test 3 compare 1
test6 = lookup_aux test 5 compare 1

loookup :: Dictionary a b -> a -> b
loookup (Dict root cmp def) key = lookup_aux root key cmp def

update_aux key value Empty _ = Node key value Empty Empty
update_aux key value (Node k v l r) cmp
  | cmp key k == EQ = Node k value l r
  | cmp key k == LT = Node k v (update_aux key value l cmp) r
  | cmp key k == GT = Node k v l (update_aux key value r cmp)

update key value (Dict root cmp def) =
  Dict (update_aux key value root cmp) cmp def

test7 = update_aux 5 5 test compare
test8 = loookup (update 1 1 test3) 1

papercuts_aux :: [Char] -> [Char] -> [([Char], [Char])] -> [([Char], [Char])]
papercuts_aux str "" list = [(str,"")]
papercuts_aux "" (x:str) list = list++[("",x:str)]++papercuts_aux [x] str list
papercuts_aux y (x:str) list = list++[(y,x:str)]++papercuts_aux (y++[x]) str list

papercuts :: [Char] -> [([Char], [Char])]
papercuts str = papercuts_aux "" str []

