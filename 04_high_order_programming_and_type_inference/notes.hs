--Anonymous functions

--suppose we want to write a function
greaterThan100 :: [Integer] -> [Integer]

--approach 1
gt100 :: Integer -> Bool
gt100 x = x > 100

greaterThan100 xs = filter gt100 xs

--Using anonymous function. Lambda abstraction
--appraoch 2
greaterThan100_2 xs = filter (\x -> x > 100) xs

--Better approach for this case
greaterThan100_3 xs = filter (> 100) xs

-- (> 100) is an operator section: if ? is an operator, the (?y) is equivalent to the function
-- \x -> x ? y and (y?) is equivalent to \x -> y ? x. In other words, using an operator section
--allows us to partially apply an operator to one of its two argumentds.


--Function composition

. :: (b -> c) -> (a -> b) -> (a -> c)
. f g = \x -> f (g x)

--same as
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

--example
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

--This version makes much clearer. myTest' is just a 'pipeline' composed of three smaller function.


--Currying and partial application

--all functions in Haskell take only one argument.
--function arrows assoicate to the right.
-- W -> X -> Y -> Z is equivalent to W -> (X -> (Y -> Z))

--function application is left-assoicative.
-- f 3 2 is really shorthand for (f 3) 2

--The "multi-argument" lambda abstraction
-- \x y z -> ... is really syntax sugar for \x -> (\y -> (\z -> ...)).
-- same for function definition f x y z = ... is syntax sugar for f = \x -> (\y -> (\z -> ...))

--If we really want a function of two arguments, we can use a single argument which is a tuple.
f'' ::(Int, Int) -> Int
f'' (x,y) = 2*x + y


--Standard Library defines function called curry and uncurry.
schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y


--uncurry is particularly useful when u have a pair and want to apply a function to it.
uncurry (+) (2, 3) --5

--Partial function
-- The idea of partyial application is that we can take a function of multiple arguments and
-- and apply it to just some of its arguments, and get out a function of the remaining arguments.

--The arguments should be ordered from "least to greatest variation", that is, arguments
--which will often be the same should be listed first, and arguments which will often be diffeent
-- should come last


--Wholemeal programming
foobar :: [Integer] -> Integer
foobar []
foobar (x:xs)
  | x > 3 = (7*x + 2) + foobar xs
  | otherwise = foobar xs

--problem
-- doing too much at once
-- wokring at too low of a level

--instead of thinkg about whtat we want to do with each elemnent, we can instead thinkg about 
--making incremental transformation to the entire input.
foobar' :: [Integer] -> Integer
foobar' = sum . map(\x -> y*x + 2) . filter (>3)

--Point-free Style
--Defining a function without reference to its arguments -- in some sense saying what a 
--function is rather than what it does


--Fold
fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)
