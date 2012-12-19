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

import HUnit

data Nil = Nothing

-- An abstract data type for dictionaries, i.e., key-value stores,
-- represented as an ordered tree.
data Nod k v = Nod k v (Nod k v) (Nod k v) | Nil

--instance Show Color where
--    show Red   = "Red"
--    show Green = "Green"
--    show Blue  = "Blue"
    
--instance Show (Nod k v) where
--    show Nil = "Nil"
--    show (Nod k v l r) = "k " ++ show k ++ ", v " ++ show v ++
--                          ", l (" ++ show l ++
--                          "), r (" ++ show r ++ ")"

--instance Show (Nod k v) where
--  show (Nod k v l r) = showNod (Nod k v l r)
--  show Nil = showNod Nil

data Dict k v cmp = Root v (Nod k v) cmp

--instance Show (Dict k v cmp) where
--  show (Root d n cmp) = "{Dict: " ++ show n ++ "}"

    

-- create a new empty dictionary with compare being the comparison function to be used for keys. The comparison function should take two keys and return one of the constants LT, EQ, GT to express the relationship between the keys. d is the default value that should be returned if a key is not found.
-- createDictionary :: cmp v -> Dict k v cmp -- TODO change cmp to function type
createDictionary :: c -> b -> Dict x b c -- FIXME
createDictionary compare d = Root d Nil compare

-- find value for key.
find :: k -> Dict k v c -> v
find key (Root d Nil _) = d
find key (Root d (Nod k v left right) cmp) = v -- FIXME

-- return a new dictionary where key now maps to value, regardless of if it was present before.
update :: k -> v -> Dict k v (k -> k -> Ordering) -> Dict k v (k -> k -> Ordering)
update key value (Root d Nil cmp) = (Root d (Nod key value Nil Nil) cmp)
update key value (Root d node cmp) = (Root d (update_node key value node cmp) cmp)
    
--update_node :: k -> v -> Nod k v -> c -> Nod k v
update_node :: k -> v -> Nod k v -> (k -> k -> Ordering) -> Nod k v
update_node key value (Nod k v Nil Nil) cmp = (Nod k v Nil Nil)
update_node key value (Nod k v Nil right) cmp
    | cmp key k == EQ = (Nod k value Nil right)
    | cmp key k == LT = (Nod k v (Nod key value Nil Nil) right)
    | cmp key k == GT = (Nod k v Nil (update_node key value right cmp))
    
update_node key value (Nod k v left Nil) cmp
    | cmp key k == EQ = (Nod k value left Nil)
    | cmp key k == LT = (Nod k v (update_node key value left cmp) Nil)
    | cmp key k == GT = (Nod k v left (Nod key value Nil Nil))
    
update_node key value (Nod k v left right) cmp
    | cmp key k == EQ = (Nod k value left right)
    | cmp key k == LT = (Nod k v (update_node key value left cmp) right)
    | cmp key k == GT = (Nod k v left (update_node key value right cmp))

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
papercuts :: [a] -> [([a], [a])] -- TODO clarify if a can be anything other than Char
papercuts s = papercutshelper s (length s) [] -- FIXME

-- trivial solution
papercutshelper :: [a] -> Int -> [([a], [a])] -> [([a], [a])] 
papercutshelper s count acc
    | count < 0 = acc
    | count >= 0 = (papercutshelper s (count-1) ((take count s, drop count s):acc))



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

-- Tests. See http://hunit.sourceforge.net/ and possibly
-- http://hackage.haskell.org/package/QuickCheck-2.1.1.1
-- for a description of tools to use.
--
-- Run from the Haskell interpreter prompt by applying the function runTestTT.
-- (The "TT" suggests text orientation with output to the terminal.)
--
-- Example: runTestTT papercutsTests

papercutsTests = TestList [TestLabel "test1" (TestCase (assertEqual ""
    [("","hello"), ("h","ello"), ("he","llo"), ("hel","lo"),
        ("hell","o"), ("hello","")]
    (papercuts "hello"))),
                           TestLabel "test2" (TestCase (assertEqual ""
    [("","")]
    (papercuts ""))),
                           TestLabel "test3" (TestCase (assertEqual ""
    [([],[0,1]), ([0],[1]), ([0,1],[])]
    (papercuts [0, 1]))),
                           TestLabel "test4" (TestCase (assertEqual ""
    [("","A rather long word, we want to be able to handle it."),
     ("A"," rather long word, we want to be able to handle it."),
     ("A ","rather long word, we want to be able to handle it."),
     ("A r","ather long word, we want to be able to handle it."),
     ("A ra","ther long word, we want to be able to handle it."),
     ("A rat","her long word, we want to be able to handle it."),
     ("A rath","er long word, we want to be able to handle it."),
     ("A rathe","r long word, we want to be able to handle it."),
     ("A rather"," long word, we want to be able to handle it."),
     ("A rather ","long word, we want to be able to handle it."),
     ("A rather l","ong word, we want to be able to handle it."),
     ("A rather lo","ng word, we want to be able to handle it."),
     ("A rather lon","g word, we want to be able to handle it."),
     ("A rather long"," word, we want to be able to handle it."),
     ("A rather long ","word, we want to be able to handle it."),
     ("A rather long w","ord, we want to be able to handle it."),
     ("A rather long wo","rd, we want to be able to handle it."),
     ("A rather long wor","d, we want to be able to handle it."),
     ("A rather long word",", we want to be able to handle it."),
     ("A rather long word,"," we want to be able to handle it."),
     ("A rather long word, ","we want to be able to handle it."),
     ("A rather long word, w","e want to be able to handle it."),
     ("A rather long word, we"," want to be able to handle it."),
     ("A rather long word, we ","want to be able to handle it."),
     ("A rather long word, we w","ant to be able to handle it."),
     ("A rather long word, we wa","nt to be able to handle it."),
     ("A rather long word, we wan","t to be able to handle it."),
     ("A rather long word, we want"," to be able to handle it."),
     ("A rather long word, we want ","to be able to handle it."),
     ("A rather long word, we want t","o be able to handle it."),
     ("A rather long word, we want to"," be able to handle it."),
     ("A rather long word, we want to ","be able to handle it."),
     ("A rather long word, we want to b","e able to handle it."),
     ("A rather long word, we want to be"," able to handle it."),
     ("A rather long word, we want to be ","able to handle it."),
     ("A rather long word, we want to be a","ble to handle it."),
     ("A rather long word, we want to be ab","le to handle it."),
     ("A rather long word, we want to be abl","e to handle it."),
     ("A rather long word, we want to be able"," to handle it."),
     ("A rather long word, we want to be able ","to handle it."),
     ("A rather long word, we want to be able t","o handle it."),
     ("A rather long word, we want to be able to"," handle it."),
     ("A rather long word, we want to be able to ","handle it."),
     ("A rather long word, we want to be able to h","andle it."),
     ("A rather long word, we want to be able to ha","ndle it."),
     ("A rather long word, we want to be able to han","dle it."),
     ("A rather long word, we want to be able to hand","le it."),
     ("A rather long word, we want to be able to handl","e it."),
     ("A rather long word, we want to be able to handle"," it."),
     ("A rather long word, we want to be able to handle ","it."),
     ("A rather long word, we want to be able to handle i","t."),
     ("A rather long word, we want to be able to handle it","."),
     ("A rather long word, we want to be able to handle it.","")]
    (papercuts "A rather long word, we want to be able to handle it.")))]