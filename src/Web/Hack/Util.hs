{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
{-|
  Contains useful generic functions not found elsewhere.


-}
module Web.Hack.Util where

import Char
import Data.Maybe
import List
import Monad
import Language.Haskell.TH

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd3 (a,b,c) = c
trd (a,b,c) = c


-- some help methods, should be extracted to util.hs
turn a x y = a y x

removeSpace :: String -> String
removeSpace = filter (/= ' ')

foldr1With :: [a] -> [[a]] -> [a]
foldr1With value = foldr1 (\l r -> l ++ value ++ r)

removeBreak :: String -> String
removeBreak = filter ((/= '\r') &&* (/= '\n'))

-- | Convert first character in String to lower.
--
-- > lowerFirst "Foo" == "foo"
-- > lowerFirst "BaR" == "baR"
-- > lowerFirst "g0O" == "g0O".'
lowerFirst :: String -> String
lowerFirst = convertFirst toLower

--lowerFirst "" = ""
--lowerFirst (x:xs) = (toLower x):xs

-- | Convert first character in String to upper.
--
-- > upperFirst "foo" == "Foo"
-- > upperFirst "bAr" == "BAr"
-- > upperFirst "G0O" == "G0O".'
upperFirst :: String -> String
upperFirst = convertFirst toUpper

--upperFirst "" = ""
--upperFirst (x:xs) = (toUpper x):xs

-- | Convert first element in list
--
-- > convertFirst (toUpper) "fO0" == "FO0"
convertFirst :: (a -> a) -> [a] -> [a]
convertFirst _ [] = []
convertFirst f (x:xs) = f x:xs

-- | Convert every space (' ') in a string to a blank ('_') instead. 
--
-- > spaceToBlank " " == "_"
-- > spaceToBlank " foo  " == "_foo__"
-- > spaceToBlank "b a r" == "b_a_r"
spaceToBlank :: String -> String
spaceToBlank "" = ""
spaceToBlank (x:xs) = (if x == ' ' then '_' else x) : spaceToBlank xs

-- | Splits a list @x@ of @a@ into a list of lists of @a@ at every @c@.
--
-- >  "splitBy "foo,bar" "',' == ["foo","bar"] ' 
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy c x  = if fst p == [] then [snd p] else fst p : splitBy c ( snd p )
               where p = break (== c) x

-- | Trims every element satisfying @c@ from the beginning or end of the list.
--
-- > trim (==' ') "  foo   " == "foo"
trim :: (a -> Bool) -> [a] -> [a]
trim c = reverse . dropWhile c . reverse . dropWhile c

trimWs = trim (==' ')

-- | Lambdifies a function. See '(||*)' and '(&&*)' for uses of 'lambdify'.
lambdify f a b x = f (a x) (b x)

-- | Lambdifies '(||)'.
--
-- > isBlankOrCommaChecker = (==' ') ||* (==',')
-- > isBlankOrComma = isBlankOrCommaChecker 'j'
(||*) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(||*) = lambdify (||)

-- | Lambdifies '(&&)'.
--
-- > isInRangeChecker = (>9) &&* (<30)
-- > isInRange = isInRangeChecker 17
(&&*) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&*) = lambdify (&&)

-- | 1D-2D-point-free operator. Similar to '.', but where
-- | the second function takes two (2) arguments instead of one (1).
--
-- > multAndSquare (^2) .^.. (*)
-- > 36 == multAndSqare 2 3
(^..) :: (c -> d) -> (a -> b -> c) -> a -> b-> d
(f ^.. g) a = f . g a

-- | 2D-2D-point-free operator. Similar to '.', but where
-- | both the first and the second function takes two (2) arguments instead of one (1).
-- (..^..) :: (t1 -> c -> d) -> (a -> b -> t1) -> a -> b -> c -> d
-- (..^..) f g = \a b c -> f (g a b) c

-- | 2D-1D-point-free operator. Similar to '.', but where
-- | the first function takes two (2) arguments instead of one (1).
-- (..^.) :: (t1 -> b -> c) -> (a -> t1) -> a -> b -> c
-- (..^.) f g = \a b -> f (g a) b

-- | See '(..^..').
(f ^...g ) a b = f . g a b


-- | See '(..^..').
--(f ..^... g) a b c = f (g a b c)

-- | See '(..^..').
--(f ...^... g) a b = f . g a b

-- | See '(..^..').
--f ...^. g = f . g

-- | See '(..^..').
--(f ...^.. g) a = f . g a

-- | Split a 2-tuple 'x' into a 2-stack and pass it to 'f'.
-- | The same as uncurry.
(..%) :: (a -> b -> c) -> (a,b) -> c
(..%) = uncurry 

(..%..) :: (c->d->e) -> (a->b->(c,d)) -> a -> b -> e
(f ..%.. g) a b = f ..% g a b

-- | Split a 3-tuple 'x' into a 3-stack and pass it to 'f'.
(...%) :: (a -> b -> c -> d) -> (a,b,c) -> d
(...%) f x = f (fst3 x) (snd3 x) (trd3 x)


-- | Pipes a monadic return through a non-monadic transformation function.
-- | liftM with arguments flipped.
--
-- > readIO >>* toUpper
(>>*) :: Monad m => m a -> (a -> b) -> m b
(>>*) v f = liftM f v -- v >>= (return . f)

(..@) f x = (fst f x, snd f x)

(...@) f x = (fst3 f x, snd3 f x, trd3 f x)


--(!1!2) (A a b) = a


-- patt :: 

-- | Get the variable and the name as a tuple.
-- | Useful whenever you need to print error messages or to a log
-- | as well as during testing.
--
-- > x = 10
-- > pair = $(varNamePair "x")
-- > 10 == fst pair
-- > "x" == snd pair
varNamePair :: String -> ExpQ
varNamePair name =
  return $ TupE [ LitE $ StringL name , VarE $ mkName name]
