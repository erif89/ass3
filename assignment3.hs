{--
Advanced Functional Programming, Assignment 3 - Haskell
Authors:
- Carl Carenvall <caca7037@student.uu.se>
- Emil Wall <emwa3503@student.uu.se>
--}

module Assignment3
(createDictionary,
lookup,
update,
fold,
rebalance,
samekeys,
papercuts,
permutations,
genwords
) where

-- An abstract data type for dictionaries, i.e., key-value stores,
-- represented as an ordered tree.
data Tree k v = Nil | Nod k v (Tree k v) (Tree k v)

instance (Show k, Show v) => Show (Tree k v) where
    show Nil = "Nil"
    show (Nod k v l r) = "k " ++ show k ++ ", v " ++ show v ++
                         ", l (" ++ show l ++
                         "), r (" ++ show r ++ ")"

data Dict k v = Root v (Tree k v) (k -> k -> Ordering)

instance (Show k, Show v) => Show (Dict k v) where
    show (Root d t cmp) = "{Dict default " ++ show d ++
                          ", Tree " ++ show t ++ "}"

-- Create a new empty dictionary with compare being the comparison function to
-- be used for keys. The comparison function should take two keys and return
-- one of the constants LT, EQ, GT to express the relationship between the
-- keys. d is the default value that should be returned if a key is not found.
createDictionary :: (k -> k -> Ordering) -> v -> Dict k v
createDictionary cmp d = Root d Nil cmp

-- Find value for key.
find :: k -> Dict k v -> v
find key (Root d t cmp) = find_node key d t cmp where
    find_node :: k -> v -> Tree k v -> (k -> k -> Ordering) -> v
    find_node _ d Nil _ = d
    find_node key d (Nod k v left right) cmp
        | cmp key k == EQ = v
        | cmp key k == LT = find_node key d left cmp
        | cmp key k == GT = find_node key d right cmp

-- Return a new dictionary where key now maps to value, regardless of if it
-- was present before.
update :: k -> v -> Dict k v -> Dict k v
update key value (Root d t cmp) = Root d (update_node key value t cmp) cmp where
    update_node :: k -> v -> Tree k v -> (k -> k -> Ordering) -> Tree k v
    update_node key value Nil _ = Nod key value Nil Nil
    update_node key value (Nod k v left right) cmp
        | cmp key k == EQ = Nod k value left right
        | cmp key k == LT = Nod k v (update_node key value left cmp) right
        | cmp key k == GT = Nod k v left (update_node key value right cmp)

-- Fold the key-value pairs of the dictionary using the function fun. fun
-- should take three arguments: key, value and sofar (in this order) which is
-- the accumulated value so far. initial is the initial value for sofar. Please
-- note that order of application is (or at least should be) not relevant.
fold :: (k -> v -> s -> s) -> Dict k v -> s -> s 
fold fun (Root d t cmp) initial = foldHelp fun t initial where
    foldHelp fun Nil acc = acc
    foldHelp fun (Nod k v left right) acc = foldHelp fun left (foldHelp fun right (fun k v acc))

-- Return a new dictionary that is more balanced (if needed). This could be run
-- when needed as part of update as well.
rebalance :: Dict k v -> Dict k v
rebalance (Root d t cmp) = (Root d (balance t) cmp) where
    balance :: Tree k v -> Tree k v
    balance Nil = Nil
    balance (Nod k v left right) =
        let
            balancedLeft = (balance left)
            balancedRight = (balance right)
            n = (Nod k v balancedLeft balancedRight)
            b = getBalance n
        in
            if b < -1 then -- right subtree too large
                balance (rotateLeft n)
            else if b > 1 then -- left subtree too large
                balance (rotateRight n)
            else -- base case (tree is balanced)
                n

    getBalance Nil = 0
    getBalance (Nod k v left right) = (getHeight left) - (getHeight right)

    getHeight Nil = 0
    getHeight (Nod k v left right) = (max (getHeight left) (getHeight right)) + 1

    rotateLeft :: Tree k v -> Tree k v
    rotateLeft Nil =
        Nil
    rotateLeft (Nod k v left Nil) =
        error "right subtree is empty"
    rotateLeft (Nod k v left (Nod k2 v2 left2 right2)) =
        (Nod k2 v2 (Nod k v left left2) right2)

    rotateRight :: Tree k v -> Tree k v
    rotateRight Nil =
        Nil
    rotateRight (Nod k v Nil right) =
        error "left subtree is empty"
    rotateRight (Nod k v (Nod k2 v2 left2 right2) right) =
        (Nod k2 v2 left2 (Nod k v right2 right))

-- Return the keys of the dictionary in a list. The order of the keys is not
-- relevant.
keys :: (Dict k v) -> [k]
keys (Root d t cmp) = allKeys t [] where
    allKeys Nil acc = acc
    allKeys (Nod k v left right) acc = allKeys left (allKeys right (k:acc))

-- Determines if dict1 and dict2 contain the same set of keys. Care has been
-- taken to make it efficient, no unnecessary large intermediate data
-- structures are contructed. samekeys uses the compare function of the first
-- dictionary, which must be type compatible with both dictionaries and return
-- an ordering for samekeys to work.
samekeys :: Dict k v -> Dict k v -> Bool
samekeys (Root d1 t1 cmp1) (Root d2 t2 cmp2) = samekeysHelp (buildstack t1 []) (buildstack t2 []) cmp1 where
    samekeysHelp stack1 stack2 cmp
        | (null stack1) || (null stack2) = (null stack1) && (null stack2)
        | otherwise = progress stack1 stack2 cmp
    progress stack1@((Nod k1 _ _ _):rest1) stack2@((Nod k2 _ _ _):rest2) cmp =
        (cmp k1 k2) == EQ && samekeysHelp (popnode stack1) (popnode stack2) cmp
    buildstack Nil acc = acc
    buildstack node@(Nod k v left _) acc = (buildstack left (node:acc))
    popnode [] = []
    popnode ((Nod _ _ _ right):stack) = buildstack right stack


-- Takes a list and returns a list of all ways to split the list into two
-- parts. Note that a part can be empty.
--
-- Example:
--   papercuts "hello" =
--     [("","hello"),("h","ello"),("he","llo"),
--      ("hel","lo"),("hell","o"),("hello","")]
papercuts :: [a] -> [([a], [a])]
papercuts s = cuts s (length s) [] where
    cuts s count acc 
        | count < 0 = acc 
        | otherwise = (cuts s (count-1) ((take count s, drop count s):acc))


-- Generates all permutations of a list. The list is generated lazily, i.e.,
-- all elements are not eagerly constructed.
--
-- Question: How can you, at least informally, verify that you are indeed
-- generating the list of permutations lazily?
--
-- Answer: take 10 (permutations "1234567890") will be very fast if the list
-- is generated lazily whereas length (permutations "1234567890") will take a
-- long time to finish because the entire list of 10! = 3628800 elements will
-- have to be evaluated.
--
-- Alternatively; first time how long it takes to return from a call to the
-- function, then time how long it takes to perform a fast operation on the
-- list generated by the function, which requires the list to be evaluated.
-- Then time the same operation again. The first and last timings should be
-- significantly faster than the second if the result is large enough, since
-- the list will be evaluated during the second timing only.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations s = [hd:res | hd:tl <- extract s, res <- permutations tl] where
    extract [] = []
    extract l@(hd:tl) = l : [first:hd:rest | first:rest <- extract tl]

-- Generates a list of all strings that can be constructed using the elements
-- of an alphabet.
--
-- Example: take 15 (genwords "ab") = ["","a","b","aa","ab","ba","bb",
--                                     "aaa","aab","aba","abb","baa",
--                                     "bab","bba","bbb"]
genwords :: [a] -> [[a]]
genwords alphabet = bar alphabet 0 where
    bar :: [a] -> Int -> [[a]]
    bar alphabet n = (foo alphabet n) ++ (bar alphabet (n+1))
    foo :: [a] -> Int -> [[a]]
    foo alphabet 0 = [[]]
    foo alphabet n = [c:res | c <- alphabet, res <- foo alphabet (n-1)]
