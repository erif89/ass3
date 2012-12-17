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

data Nil = Nothing

-- An abstract data type for dictionaries, i.e., key-value stores,
-- represented as an ordered tree.
data Node k v = Node k v (Node k v) (Node k v) | Nil

--instance Show Color where
--    show Red   = "Red"
--    show Green = "Green"
--    show Blue  = "Blue"
    
--instance Show (Node k v) where
--    show Nil = "Nil"
--    show (Node k v l r) = "k " ++ show k ++ ", v " ++ show v ++
--                          ", l (" ++ show l ++
--                          "), r (" ++ show r ++ ")"

--instance Show (Node k v) where
--  show (Node k v l r) = showNode (Node k v l r)
--  show Nil = showNode Nil

data Dict k v cmp = Root v (Node k v) cmp

--instance Show (Dict k v cmp) where
--  show (Root d n cmp) = "{Dict: " ++ show n ++ "}"

    

-- create a new empty dictionary with compare being the comparison function to be used for keys. The comparison function should take two keys and return one of the constants LT, EQ, GT to express the relationship between the keys. d is the default value that should be returned if a key is not found.
-- createDictionary :: cmp v -> Dict k v cmp -- TODO change cmp to function type
createDictionary :: c -> b -> Dict x b c -- FIXME
createDictionary compare d = Root d Nil compare

-- find value for key.
find :: k -> Dict k v c -> v
find key (Root d Nil _) = d
find key (Root d (Node k v left right) cmp) = v -- FIXME

-- return a new dictionary where key now maps to value, regardless of if it was present before.
update :: k -> v -> Dict k v (k -> k -> Ordering) -> Dict k v (k -> k -> Ordering)
update key value (Root d Nil cmp) = (Root d (Node key value Nil Nil) cmp)
update key value (Root d node cmp) = (Root d (update_node key value node cmp) cmp)
    
--update_node :: k -> v -> Node k v -> c -> Node k v
update_node :: k -> v -> Node k v -> (k -> k -> Ordering) -> Node k v
update_node key value (Node k v Nil Nil) cmp = (Node k v Nil Nil)
update_node key value (Node k v Nil right) cmp
    | cmp key k == EQ = (Node k value Nil right)
    | cmp key k == LT = (Node k v (Node key value Nil Nil) right)
    | cmp key k == GT = (Node k v Nil (update_node key value right cmp))
    
update_node key value (Node k v left Nil) cmp
    | cmp key k == EQ = (Node k value left Nil)
    | cmp key k == LT = (Node k v (update_node key value left cmp) Nil)
    | cmp key k == GT = (Node k v left (Node key value Nil Nil))
    
update_node key value (Node k v left right) cmp
    | cmp key k == EQ = (Node k value left right)
    | cmp key k == LT = (Node k v (update_node key value left cmp) right)
    | cmp key k == GT = (Node k v left (update_node key value right cmp))

-- fold the key-value pairs of the dictionary using the function fun. fun should take three arguments: key, value and sofar (in this order) which is the accumulated value so far. initial is the initial value for sofar. Please note that order of application is (or at least should be) not relevant.
fold :: (k -> v -> s) -> Dict k v c -> s -> s 
fold fun dict initial = initial -- FIXME

-- return a new dictionary that is more balanced (if needed). This could be run when needed as part of update as well.
-- rebalace :: (Dict k v cmp) -> (Dict k v cmp) FIXME
rebalance :: Dict k v c -> Dict k v c  -- FIXME
rebalance dict = dict -- FIXME

-- return the keys of the dictionary in a list. The order of the keys is not relevant.
keys :: (Dict k v cmp) -> [k]
keys dict = [] -- FIXME

-- determines if dict1 and dict2 contain the same set of keys. Take care to make it efficient, i.e., do not construct unnecessary large intermediate data structures. samekeys should use the compare function, which should be the same for the two dictionaries and can be assumed to behave 'in the right way' for its two arguments (i.e. EQ return value implies that the arguments can be swapped and the result is still EQ, GT implies that if the arguments are swapped the return value will be LT and vice-versa).
-- samekeys :: (Dict k v cmp) (Dict k v cmp) -> Bool -- FIXME
samekeys :: Dict k v c -> Dict k v c -> Bool -- FIXME
samekeys dict1 dict2 = False -- FIXME


-- Takes a list and returns a list of all ways to split the list into two parts. Note that a part can be empty.
-- Example:
--   papercuts "hello" =
--     [("","hello"),("h","ello"),("he","llo"),
--      ("hel","lo"),("hell","o"),("hello","")]
papercuts :: String -> [(String, String)] -- TODO clarify if a can be anything other than Char
papercuts s = papercutshelper s (length s) (length s) [] -- FIXME

-- trivial solution
papercutshelper :: String -> Int -> Int -> [(String, String)] -> [(String, String)] 
papercutshelper s count length acc
    | count < 0 = acc
    | count >= 0 = (papercutshelper s (count-1) length ((take count s, drop count s):acc))



-- Generates all permutations of a list. The list is generated lazily, i.e., all elements are not eagerly constructed.
-- Question: How can you, at least informally, verify that you are indeed generating the list of permutations lazily?
-- Answer: TODO
permutations :: [a] -> [[a]]
permutations s = [s] -- FIXME


-- Generates a list of all strings that can be constructed using the elements of an alphabet.
-- Examples:
--   take 10 (genwords "ab") =
--     ["","a","b","aa","ba","ab","bb","aaa","baa","aba"]
--   take 20 (genwords "lisp") =
--     ["","l","i","s","p","ll","il","sl","pl","li","ii","si","pi","ls","is",
--      "ss","ps","lp","ip","sp"]
--   take 20 (filter (\w -> (length w) == 4) (genwords "dea")) =
--     ["dddd","eddd","addd","dedd","eedd","aedd","dadd","eadd","aadd","dded",
--      "eded","aded","deed","eeed","aeed","daed","eaed","aaed","ddad","edad"]
genwords :: String -> [String]
genwords s = [""] -- FIXME

-- TODO add tests. See http://hunit.sourceforge.net/ and http://hackage.haskell.org/package/QuickCheck-2.1.1.1