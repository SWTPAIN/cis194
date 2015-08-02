--enum
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

--Pattern Matching
isSmall :: Thing -> Bool
isSmall Shoe        = True
isSmall Ship        = False
isSmall SealingWax  = True
isSmall Cabbage     = True
isSmall King        = False

--shorter version
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

data FailableDouble = Failure
                    | OK Double
  deriving Show


safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person = Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

--an algebraic data type has one or more data constructors, and each data constructor can have zero or more arguments.
--data AlgDataType = Constr1 Type11 Type12
--                 | Constr2 Type21
--                 | Constr3 Type31 Type32 Type33
--                 | Constr4

--more about pattern mathching
--An underscore can be used as wildcard pattern
--A pattern of the form x@pat can be used to match a value aginst the pattern pat, but also give the name x to the entire value being matched. Ex.
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

--Pattern Matching Grammar
--pat ::= _
--      | var
--      | var @ ( pat )
--      | (Constructor pat1 pat2 .. patn)

--literval values like 2 or 'c' can be thought of as constructors with no arguments.
-- data Int  = 0 | 1 | -1 | 2 | -2 | ...
-- data Chat = 'a' | 'b' | 'c' | ... 
-- Therefore we can pattern-match against literal values.

--Case expressions
--case exp of
--  pat1 -> exp1
--  pat2 -> exp2
--  ...

ex03 = case "Hello" of
         []       -> 3
         ('H':s)  -> length s
         _        -> 7

--In fact, the syntax for defining functions we have seen is really just 
--convenient syntax sugar for defining a case expression.
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

--Recursive data types
--Date types can be recursive, that is, defined in terms of themselves.
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty       = 1
intListProd (Cons x l)  = x * intListProd l

data Tree = Leaf Chat
          | Node Tree Int Tree
