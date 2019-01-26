{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Exercises where

import Data.Monoid (mconcat, (<>))
import Data.Tuple (swap)

{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CNil :: CountableList
  CCons :: Countable a => a -> CountableList -> CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList = \case
  CNil -> 0
  CCons x xs -> (count x) + countList xs

-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero = \case
  CNil -> CNil
  CCons x xs
    | count x == 0 -> xs'
    | otherwise    -> CCons x xs'
    where
      xs' = dropZero xs

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"

-- Impossible - from scrutinising a CountableList we only learn a is an instance of Countable

{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  ANil :: AnyList
  ACons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList = reverseAnyList' ANil
  where
    reverseAnyList' stack = \case
      ANil       -> stack
      ACons x xs -> reverseAnyList' (ACons x stack) xs

-- Impossible, as the 'a'(s) inside the AnyList is existentially quanitified (so
-- not necessarily the same as the a in the predicate
filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList = \case
  ANil       -> 0
  ACons _ xs -> 1 + lengthAnyList xs

-- Again, not the same m in the list...
foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList = \case
  ANil -> True
  _    -> False

instance Show AnyList where
  show = error "What about me?"

-- ^ I don't even know what this is asking for
-- quite clearly we can't show the elements of the list

{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- `input` is existential - we can only apply the function to it, to get an
-- `output`

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

-- We could, provided that `output` had an instance e.g.

instance Show output => Show (TransformableTo output) where
  show (TransformWith f i) = show (f i)

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

-- Sure... (it's basically the same as (->) r) e.g.

instance Functor TransformableTo where
  fmap g (TransformWith f i) = TransformWith (g . f) i

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

eq :: EqPair -> Bool
eq (EqPair x y) = x == y

neq :: EqPair -> Bool
neq (EqPair x y) = x /= y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- We still need a GADT to provide the EQ constraint

{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ mb) = getInt mb

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers = \case
  EmptyBox       -> 0
  IntBox _ mb    -> countLayers mb
  StringBox _ mb -> countLayers mb
  BoolBox _ mb   -> countLayers mb

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- This might be cheating... (not possible otherwise)
type family Unwrap t where
  Unwrap Int    = ()
  Unwrap String = Int
  Unwrap Bool   = String

unwrapBox :: MysteryBox a -> Maybe (MysteryBox (Unwrap a))
unwrapBox = \case
  EmptyBox       -> Nothing 
  IntBox _ mb    -> Just mb
  StringBox _ mb -> Just mb
  BoolBox _ mb   -> Just mb

{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head :: HList (t, ts) -> t
head (HCons x _ ) = x 

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- Not possible, as not of the form (head, tail)
-- `HList (Int, (String, (Bool, ()))) -> Int` would work though

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- Probably cheating again...
type family Append xs ys where
  Append ()      ys = ys
  Append (x, xs) ys = (x, Append xs ys)

append :: HList xs -> HList ys -> HList (Append xs ys)
append HNil         ys = ys
append (HCons x xs) ys = HCons x (append xs ys)

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  Leaf :: HTree Empty
  Node :: HTree l -> a -> HTree r -> HTree (Branch l a r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch l c r) -> HTree (Branch Empty c r)
deleteLeft (Node _ c r) = Node Leaf c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. Recursion is your friend here - you
-- shouldn't need to add a constraint to the GADT!

-- Cheating again with FlexibleInstances/FlexibleContexts
-- Genuinely not sure how to do it otherwise
instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  (Node l c r) == (Node l' c' r')
    = (l == l') && (c == c') && (r == r')

{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ALNil :: AlternatingList a b
  ALCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts = \case
  ALNil       -> []
  ALCons x xs -> x : getSeconds xs

getSeconds :: AlternatingList a b -> [b]
getSeconds = \case
  ALNil       -> []
  ALCons _ xs -> getFirsts xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
{-
foldValues al
  = let xs = getFirsts al
        ys = getSeconds al
    in (mconcat xs, mconcat ys)
-}
foldValues = \case
  ALNil -> (mempty, mempty)
  ALCons x xs -> swap $ (mempty, x) <> foldValues xs

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval = \case
  Equals x y    -> (eval x) == (eval y)
  Add x y       -> (eval x) + (eval y)
  If b e e'
    | eval b    -> eval e
    | otherwise -> eval e'
  IntValue x    -> x
  BoolValue b   -> b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse = \case
  DirtyAdd    x y  -> Add <$> parse x <*> parse y
  DirtyIntValue x  -> Just $ IntValue x
  _                -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe'?

-- I don't really understand the question

{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TANil :: TypeAlignedList a a
  TACons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

runList :: TypeAlignedList a b -> a -> b
runList = \case
  TANil      -> id
  TACons f l -> runList l . f

-- | b. Which types are existential?

-- The intermediate ones (e.g. type variable `b` in TACons)

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs l l' 
  = TACons (runList l . runList l') TANil

