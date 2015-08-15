--Polymorphic data types
data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

--Polymorphic functions

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

--Total & Partial functions
--Functions which have certain input that will make them recurse infinitely are called partial.
--In Prelude, head, tail, init, last and (!!) are all partial functions
--Function which are well-defined on all possible inputs are known as total functions

--Good practice to avoid partial functions as much as possible

--Writing partial functions
--Chnage the output type of the function to indicate the possible failure
data Maybe a = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

--Advantages
--1. saftHead will never crash
--2. The type of safeHead makes it obvious that it may fail for some inputs
--3. The type sysmte ensures that users of safeHead must appropriately check the return value of safeHead to see whether they got a value or Nothing

--safeHead is still "partial" in some senese. But it reflects the partiality in the type system, so it is now safe. The goal is to have the types tell us as much as possible about the behaviour of functions.

--If we are sure some condition guaranteed. The type ought to reflect the guarantee
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []   = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
