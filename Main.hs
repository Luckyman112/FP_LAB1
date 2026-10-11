-- Laboratory work N1, Haskell, variant 2.
-- Run: runghc Main.hs
-- Every task prints its result, some debug lines and [PASS]/[FAIL] checks.
module Main where

import Data.Char (isAlpha, isSpace, toUpper)
import Data.List (permutations)
import System.Exit (exitFailure)
import System.IO

-- Prints the check result and returns it, so failures can be counted.
check :: String -> Bool -> IO Bool
check label ok = do
  putStrLn ((if ok then "  [PASS] " else "  [FAIL] ") ++ label)
  pure ok

title :: String -> IO ()
title t = putStrLn ("\n=== " ++ t ++ " ===")

------------------------------------------------------------------------------
-- TASK 1 (Solution 1): sort the list descending and take the first element
------------------------------------------------------------------------------

-- Own merge sort in descending order.
sortDesc :: Ord a => [a] -> [a]
sortDesc [] = []
sortDesc [x] = [x]
sortDesc xs = merge (sortDesc left) (sortDesc right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge as [] = as
    merge (a : as) (b : bs)
      | a >= b = a : merge as (b : bs)
      | otherwise = b : merge (a : as) bs

describeMaximum :: (Ord a, Show a) => [a] -> String
describeMaximum xs =
  case sortDesc xs of
    [] -> "The list is empty, there is no maximum number"
    (m : _) -> "The maximum number is " ++ show m

------------------------------------------------------------------------------
-- TASK 2 (Solution 1): split into chars -> reverse -> join
------------------------------------------------------------------------------

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

reverseName :: String -> Either String String
reverseName raw
  | null name = Left "the name is empty"
  | not (all validChar name) = Left "the name may contain only letters, spaces, '-' and '''"
  | otherwise = Right (concat reversed) -- step 3: join the letters back
  where
    name = trim raw
    validChar c = isAlpha c || c `elem` " -'"
    letters = map (: []) name -- step 1: "Ann" -> ["A","n","n"]
    reversed = foldl (flip (:)) [] letters -- step 2: reverse the order

describeReversedName :: String -> String
describeReversedName raw =
  either (\err -> "Error: " ++ err) ("The reversed name is " ++) (reverseName raw)

------------------------------------------------------------------------------
-- TASK 3 (Solution 2): reducer (current text, new word) -> new text
------------------------------------------------------------------------------

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : cs

capitalizeReducer :: String -> String -> String
capitalizeReducer "" word = capitalize word
capitalizeReducer text word = text ++ " " ++ capitalize word

capitalizeSentence :: String -> String
capitalizeSentence = foldl capitalizeReducer "" . words

------------------------------------------------------------------------------
-- TASK 4: number of solutions of h*x^2 + i*x + j = 0, Nothing = infinitely many
------------------------------------------------------------------------------

numbOfSol :: Double -> Double -> Double -> Maybe Int
numbOfSol h i j =
  case (isZero h, isZero i, isZero j) of
    (True, True, True) -> Nothing -- 0 = 0
    (True, True, False) -> Just 0 -- j = 0, j /= 0
    (True, False, _) -> Just 1 -- linear equation
    _ -> case compareToZero (i * i - 4 * h * j) of
      LT -> Just 0
      EQ -> Just 1
      GT -> Just 2
  where
    isZero x = abs x < 1e-12
    -- Relative tolerance: in floating point 0.2^2 - 4*0.1*0.1 is not exactly 0.
    compareToZero d
      | abs d <= 1e-9 * max 1 (i * i) = EQ
      | otherwise = compare d 0

------------------------------------------------------------------------------
-- TASK 5: run-length encoding
------------------------------------------------------------------------------

runLengthEncode :: Eq a => [a] -> [(Int, a)]
runLengthEncode = foldr step []
  where
    step x ((n, y) : rest) | x == y = (n + 1, y) : rest
    step x encoded = (1, x) : encoded

------------------------------------------------------------------------------
-- TASK 6: remove from the first list all elements of the second list
------------------------------------------------------------------------------

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll [] _ = []
removeAll (x : xs) toRemove
  | x `elem` toRemove = removeAll xs toRemove
  | otherwise = x : removeAll xs toRemove

------------------------------------------------------------------------------
-- TASK 7: binary search trees
------------------------------------------------------------------------------

data Tree a = Branch a (Tree a) (Tree a) | Leaf
  deriving (Eq, Show)

-- Ord is needed to compare values. Every node must lie in the interval
-- (lower, upper] given by ALL its ancestors, not only by its parent.
-- Values <= node are on the left, values > node on the right (as insertTree).
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree = go Nothing Nothing
  where
    go _ _ Leaf = True
    go lower upper (Branch x l r) =
      maybe True (< x) lower
        && maybe True (x <=) upper
        && go lower (Just x) l
        && go (Just x) upper r

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x li re)
  | e <= x = Branch x (insertTree e li) re
  | otherwise = Branch x li (insertTree e re)

-- Mutant 1: `<` instead of `<=`, equal values go to the right.
insertEqualRight :: Ord a => a -> Tree a -> Tree a
insertEqualRight e Leaf = Branch e Leaf Leaf
insertEqualRight e (Branch x li re)
  | e < x = Branch x (insertEqualRight e li) re
  | otherwise = Branch x li (insertEqualRight e re)

-- Mutant 2: drops the right subtree when going left (order stays correct!).
insertLosesRight :: Ord a => a -> Tree a -> Tree a
insertLosesRight e Leaf = Branch e Leaf Leaf
insertLosesRight e (Branch x li _)
  | e <= x = Branch x (insertLosesRight e li) Leaf
insertLosesRight e (Branch x li re) = Branch x li (insertLosesRight e re)

-- Property: isSearchTree t ==> isSearchTree (insert v t).
-- Returns the first counterexample, if there is one.
findCounterexample :: (Int -> Tree Int -> Tree Int) -> Maybe (Int, Tree Int)
findCounterexample insertFn =
  case [(v, t) | t <- testTrees, isSearchTree t, v <- [0 .. 6],
                 not (isSearchTree (insertFn v t))] of
    [] -> Nothing
    (c : _) -> Just c
  where
    -- trees from every subset of [1,1,2,3] (small ones first, so the
    -- counterexample is short) and from every permutation of [1..5]
    testTrees = map (foldr insertTree Leaf) (subsets [1, 1, 2, 3] ++ permutations [1 .. 5])
    subsets = foldr (\x acc -> acc ++ map (x :) acc) [[]]

------------------------------------------------------------------------------
-- MAIN: results and checks
------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8

  title "TASK 1. Maximum number"
  let numbers = [5, 10, 11, 15, 3, 8] :: [Int]
  putStrLn ("  [DEBUG] sorted descending: " ++ show (sortDesc numbers))
  putStrLn ("  " ++ describeMaximum numbers)
  r1 <- sequence
    [ check "max of [5,10,11,15,3,8] is 15" (describeMaximum numbers == "The maximum number is 15")
    , check "empty list" (describeMaximum ([] :: [Int]) == "The list is empty, there is no maximum number")
    , check "negative numbers" (describeMaximum [-3, -1, -2 :: Int] == "The maximum number is -1")
    ]

  title "TASK 2. Reverse a name"
  putStrLn ("  [DEBUG] letters: " ++ show (map (: []) "Sergej"))
  mapM_ (putStrLn . ("  " ++) . describeReversedName) ["Sergej", "  Anna-Maria ", "", "R2D2"]
  r2 <- sequence
    [ check "\"Sergej\" -> \"jegreS\"" (reverseName "Sergej" == Right "jegreS")
    , check "spaces are trimmed" (reverseName "  Ann " == Right "nnA")
    , check "empty name and digits are rejected"
        (all (either (const True) (const False) . reverseName) ["", "   ", "R2D2"])
    ]

  title "TASK 3. Capitalize every word (reducer)"
  let sentence = "haskell is  a functional language"
  mapM_ (\acc -> putStrLn ("  [DEBUG] accumulator: " ++ show acc))
    (scanl capitalizeReducer "" (words sentence))
  putStrLn ("  Result: " ++ capitalizeSentence sentence)
  r3 <- sequence
    [ check "reducer appends a capitalized word" (capitalizeReducer "Hello" "world" == "Hello World")
    , check "extra spaces are removed" (capitalizeSentence "  hi   there " == "Hi There")
    , check "empty sentence" (capitalizeSentence "" == "")
    ]

  title "TASK 4. Number of solutions of h*x^2 + i*x + j = 0"
  let equations = [(1, -3, 2), (1, 2, 1), (1, 0, 1), (0, 2, -4), (0, 0, 5), (0, 0, 0), (0.1, 0.2, 0.1)]
  mapM_ (\(h, i, j) -> putStrLn ("  numbOfSol " ++ unwords (map show [h, i, j])
                                   ++ " = " ++ show (numbOfSol h i j))) equations
  r4 <- check "expected answers (Nothing = infinitely many)"
    (map (\(h, i, j) -> numbOfSol h i j) equations
       == [Just 2, Just 1, Just 0, Just 1, Just 0, Nothing, Just 1])

  title "TASK 5. Run-length encoding"
  print (runLengthEncode [1, 2, 2, 3, 2, 4 :: Int])
  r5 <- sequence
    [ check "[1,2,2,3,2,4]" (runLengthEncode [1, 2, 2, 3, 2, 4 :: Int] == [(1, 1), (2, 2), (1, 3), (1, 2), (1, 4)])
    , check "decoding gives the original list"
        (concatMap (uncurry replicate) (runLengthEncode "aaabccdddd") == "aaabccdddd")
    , check "empty list" (null (runLengthEncode ([] :: [Int])))
    ]

  title "TASK 6. Remove elements of the second list"
  print (removeAll [1, 2, 2, 3, 4, 5, 4, 6 :: Int] [2, 4, 7])
  r6 <- sequence
    [ check "[1,2,2,3,4,5,4,6] minus [2,4,7]" (removeAll [1, 2, 2, 3, 4, 5, 4, 6 :: Int] [2, 4, 7] == [1, 3, 5, 6])
    , check "strings" (removeAll "hello world" "lo" == "he wrd")
    , check "empty second list" (removeAll [1, 2, 3 :: Int] [] == [1, 2, 3])
    ]

  title "TASK 7. Binary search trees"
  let bad = Branch 5 (Branch 3 Leaf (Branch 9 Leaf Leaf)) Leaf :: Tree Int
      mutants = [("original insertTree", insertTree), ("mutant 1: equal values go right", insertEqualRight),
                 ("mutant 2: loses right subtree", insertLosesRight)]
  putStrLn ("  isSearchTree " ++ show bad ++ " = " ++ show (isSearchTree bad))
  mapM_ (\(name, f) -> putStrLn ("  " ++ name ++ ": " ++
           maybe "property holds" (\(v, t) -> "counterexample: insert " ++ show v ++ " into " ++ show t)
                 (findCounterexample f))) mutants
  r7 <- sequence
    [ check "9 in the left subtree of 5 is detected" (not (isSearchTree bad))
    , check "original insertTree keeps the property" (findCounterexample insertTree == Nothing)
    , check "mutant 1 is detected" (findCounterexample insertEqualRight /= Nothing)
    , check "mutant 2 is NOT detected by this property" (findCounterexample insertLosesRight == Nothing)
    ]

  title "TASK 8. map f . concat = concat . map (map f) (proof in README.md)"
  let xss = [[1, 2], [3], [], [4, 5]] :: [[Int]]
  putStrLn ("  left:  " ++ show ((map (* 10) . concat) xss))
  putStrLn ("  right: " ++ show ((concat . map (map (* 10))) xss))
  r8 <- check "both sides are equal" ((map (* 10) . concat) xss == (concat . map (map (* 10))) xss)

  let results = concat [r1, r2, r3, [r4], r5, r6, r7, [r8]]
  title ("SUMMARY: " ++ show (length (filter id results)) ++ " of " ++ show (length results) ++ " checks passed")

  -- Interactive part: tasks 2 and 3 accept user input.
  putStrLn "\nEnter a name:"
  name <- filter (/= '\xFEFF') <$> getLine -- drop BOM added by PowerShell pipes
  putStrLn (describeReversedName name)
  putStrLn "Enter a sentence:"
  input <- getLine
  putStrLn ("Capitalized: " ++ capitalizeSentence input)

  if and results then pure () else exitFailure
