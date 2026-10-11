# Laboratory Work N1, Haskell, Variant 2

Run the program:

    runghc Main.hs

The program prints the result of every task and simple checks. At the end it asks for a name and a sentence.

## Tasks

Task 1. The list is sorted in descending order with merge sort, and the first element is the maximum. For an empty list a message is printed.

Task 2. The name is split into letters, the order is reversed, and the letters are joined back. An empty name or a name with digits is rejected.

Task 3. A reducer takes the current text and a new word and appends the word with a capital first letter. foldl applies it to all words.

Task 4. numbOfSol uses case to count the solutions of h*x^2 + i*x + j = 0. Nothing means infinitely many solutions (0 = 0). The discriminant is compared with a small tolerance because of floating-point rounding.

Types (in GHCi use `:t`, for example `:t tail`):

    tail      :: [a] -> [a]
    sqrt      :: Floating a => a -> a
    pi        :: Floating a => a
    exp       :: Floating a => a -> a
    (^)       :: (Num a, Integral b) => a -> b -> a
    (/=)      :: Eq a => a -> a -> Bool
    numbOfSol :: Double -> Double -> Double -> Maybe Int

A type is written explicitly before the function, for example `numbOfSol :: Double -> Double -> Double -> Maybe Int`.

Task 5. Run-length encoding with foldr: equal neighbours are counted together.

Task 6. Elements of the first list that also appear in the second list are removed.

Task 7. isSearchTree checks every node against all its ancestors. The property "a search tree stays a search tree after insertTree" holds for the original function. Mutant 1 (equal values go right) is detected. Mutant 2 (loses the right subtree) is not detected, because a tree with fewer elements is still a valid search tree.

## Task 8. Proof of map f . concat = concat . map (map f)

First a lemma: map f (xs ++ ys) = map f xs ++ map f ys. Proof by induction on xs.

    xs = []:
      map f ([] ++ ys) = map f ys = [] ++ map f ys = map f [] ++ map f ys

    xs = x : xs':
      map f ((x : xs') ++ ys)
        = map f (x : (xs' ++ ys))
        = f x : map f (xs' ++ ys)
        = f x : (map f xs' ++ map f ys)      -- induction hypothesis
        = (f x : map f xs') ++ map f ys
        = map f (x : xs') ++ map f ys

Now the law. Proof by induction on xss.

    xss = []:
      map f (concat []) = map f [] = []
      concat (map (map f) []) = concat [] = []

    xss = xs : xss':
      map f (concat (xs : xss'))
        = map f (xs ++ concat xss')
        = map f xs ++ map f (concat xss')            -- lemma
        = map f xs ++ concat (map (map f) xss')      -- induction hypothesis
        = concat (map f xs : map (map f) xss')
        = concat (map (map f) (xs : xss'))

Both sides are equal for every list, so the law holds.
