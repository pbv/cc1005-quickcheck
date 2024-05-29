{-
  ---------------------------------------------------------------
  Testing simple equational properties
  ---------------------------------------------------------------

  "Program testing can be used to show the presence of bugs, but never
  to show their absence!"  --- Edsger Dijkstra

  "Beware of bugs in the above code; I have only proved it correct,
  not tried it." --- Donald Knuth

  To run these examples, start GHCi from the project directory
  and execute `quickCheck <property>'.

    $ cabal repl
    ...
    Loaded GHCi configuration from /home/pbv/.ghci
    [1 of 1] Compiling ProofsAndTests   ( src/ProofsAndTests.hs, interpreted )
    Ok, one module loaded.

    ghci> quickCheck prop_rev_rev
    +++ OK, passed 100 tests.
    ghci> quickCheck prop_rev_wrong 
    *** Failed! Falsified (after 3 tests and 2 shrinks):    
    [0,1]
    [1,0] /= [0,1]

  You can also use `verboseCheck' to see all random tests generated.
  -------------------------------------------------------------------
  Pedro Vasconcelos, 2024
-}
module ProofsAndTests where

import Test.QuickCheck


-- this function expresses the property that
-- for all xs :: [Int],  reverse (reverse xs) = xs
prop_rev_rev :: [Int] -> Property
prop_rev_rev xs
  = reverse (reverse xs) === xs


-- this function expresses an invalid property
-- QuickCheck can find a counterexample
prop_rev_wrong :: [Int] -> Property
prop_rev_wrong xs
  = reverse xs === xs

-- addition on integers is associative
prop_int_add_assoc :: Int -> Int -> Int -> Property
prop_int_add_assoc x y z
  = (x+y)+z === x+(y+z)

-- this fails: addition on floats is *not* associative,
-- because of floating-point rounding
prop_float_add_assoc :: Float -> Float -> Float -> Property
prop_float_add_assoc x y z
  = (x+y)+z === x+(y+z)

-- map doesn't change the length of the list:
-- length (map f xs) = length xs
-- We use `Fun a b' instead of `a -> b' for QuickCheck to be able
-- to generate and show functions
-- Such functions are allways defined by simple branches, e.g.
-- { 'a' -> 1, 'b' -> 2, _ -> 0 }
--
prop_map_len :: Fun Int Int -> [Int] -> Property
prop_map_len (Fun _ f) xs =
  length (map f xs) === length xs

-- a false property saying that filter doesn't change the length of the list
-- length (filter f xs) = length xs
--
prop_filter_len :: Fun Int Bool -> [Int] -> Property
prop_filter_len (Fun _ f) xs
  = length (filter f xs) === length xs

-- Properties about list insertion

-- The recursive function to insert into an ordered list preserving ordering
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x:y:ys
  | otherwise = y:insert x ys

-- an auxiliary function to check if a list is ordered
-- i.e. x1<=x2<=x3<= ... <=xn
ordered :: Ord a => [a] -> Bool
ordered xs = and (zipWith (<=) xs (tail xs))

-- We want to write the following property
-- "if xs is ordered then insert x xs is also ordered"
-- The first solution uses an implication  (==>)
-- the operator (==>) discards lists that not ordered
-- this discards too many tests and gives up after a while!
prop_insert_bad :: Int -> [Int] -> Property
prop_insert_bad x ys
  = ordered ys ==> ordered (insert x ys)

-- a better way of writing this property:
-- generating only ordered lists to begin with
-- the type `OrderedList a' is defined the QuickCheck library
prop_insert_good :: Int -> OrderedList Int -> Bool
prop_insert_good x (Ordered ys)
  = ordered (insert x ys)

