% CS 340: Programming Paradigms and Patterns
% Lect 08 - Defining Types and Type Classes
% Michael Lee

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module Lect08 where
import Prelude hiding (Word, Maybe, Just, Nothing, Either, Left, Right)
import Data.Char
\end{code}

Defining Types and Type Classes
===============================

Agenda:
  - Type synonyms
  - Algebraic data types
  - Polymorphic types
  - Type Classes


Type synonyms
-------------


`type` defines type synonyms, i.e., alternative names for existing types. Note
that all type names must be capitalized.


\begin{code}
type Letter = Char

type Word = [Letter]

type Sentence = [Word]

sentences :: [Word] -> [Word] -> [Word] -> [Sentence]
sentences subjs verbs objs = [[s,v,o] | s <- subjs, v <- verbs, o <- objs]

type Point2D = (Double, Double)

distance :: Point2D -> Point2D -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
\end{code}


Algebraic data types
--------------------

The `data` keyword is used to define new types. In the definition of the type we
list one or more value constructors (aka data constructors) that would be used
to create values of this type. Value constructor names must also be capitalized.

E.g., the `YesOrNo` type below has two value constructors: `Yes` and `No`:

data Type = dataConstructor1 | dataConstructor2 deriving OtherType
> data YesOrNo = Yes | No deriving Show

(the `deriving` clause allows values of this type to be displayed in GHCi 
and converted to strings --- we'll clarify this later)

A value constructor is essentially a function that returns a value of the
defined type. We can also pattern match against value constructors in 
functions:

\begin{code}
not' :: YesOrNo -> YesOrNo
not' Yes = No
not' No  = Yes


(|||) :: YesOrNo -> YesOrNo -> YesOrNo
No ||| No = No
_  ||| _  = Yes


or' :: [YesOrNo] -> YesOrNo
or' = foldr (|||) No
\end{code}

---

In the type definition, value constructors can also be followed by field types.

E.g., `Box` has a single value constructor (also named `Box` --- this is ok
because type names and functions are in separate namespaces) with three fields:

\begin{code}
data Box = Box Int Bool String deriving Show
\end{code}

To construct a `Box`, we pass its value constructor values corresponding to the
field types. 

\begin{code}
b1 = Box 5 True "hello"
b2 = Box 100 False "goodbye"
\end{code}


Use pattern matching to write some Box functions:

\begin{code}
boxStr :: Box -> String
boxStr (Box _ _ s) = s

() around Box are necessary so it's not interpretted as a parameter of boxStr
> boxStr :: Box -> String
> boxStr (Box _ _ s) = s
>
> boxCombine :: Box -> Box -> Box
> boxCombine (Box n1 b1 s1) (Box n2 b2 s2) = Box (n1+n2) (b1||b2) (s1++s2)

---

We can have multiple value constructors with varying numbers of fields. 

E.g., `Shape` has three value constructors, each with one or more fields:

Shape is a type, Circle Triangle and Rectangle are data constructors
> data Shape = Circle Double 
>              | Triangle Double Double 
>              | Rectangle Double Double deriving Show

Pattern matching lets us differentiate between different values of a given type,
and extract their constituent fields:

\begin{code}
area :: Shape -> Double
area (Circle r) = pi * r^2
area (Triangle h b) = (h*b)/2
area (Rectangle l w) = l*w
\end{code}

---

We call these "algebraic" data types because data types defined in this way can
be formed from the "sum" and "product" of other types.

Here are two sum types:

> data T1 = T1V1 | T1V2 | T1V3 -- 3 values
> data T2 = T2V1 Bool | T2V2 T1 -- 5 values

To determine the values that make up either `T1` or `T2`, we just "sum up" the
values that can be created using all their respective value constructors. How
many values make up `T2`?

Here's a product type:

> data T3 = T3V Bool T1 -- 2 *3 = 6 values

To determine the values that make up `T3`, we compute the "product" of the
values for the constituent types of its single value constructor. How many
values make up `T3`?

Here's a type that is both a sum and product type:

> data T4 = T4V1 T1 T2 | T4V2 T2 T3 --3*5 + 5*6 = 45

How many values make up `T4`?

---

We can also use "record" syntax to define attribute names and automatically
generate "getter" functions:

\begin{code}
data Student = Student { firstName :: String
                       , lastName  :: String
                       , studentId :: Integer
                       , grades    :: [Char]
                       } deriving Show
\end{code}

We can still create values with fields specified positionally:

> s1 = Student "John" "Doe" 1234 ['A', 'B']
-- firstName s1 returns "John"
Or we can specify fields by name (order doesn't matter):

\begin{code}
s2 = Student { lastName = "Doe"
             , firstName = "Jane"
             , grades = ['A', 'C']
             , studentId = 2345 }
\end{code}

Record syntax also provides a shortcut for creating a new value from another:

\begin{code}
s3 = s1 { grades = ['B', 'A', 'D'] }
\end{code}

---

We can also define *self-referential* types --- i.e., a type where one or more
value constructors reference the type being defined.

\begin{code}
data RussianDoll = RussianDoll String RussianDoll | EmptyDoll
                   deriving Show
\end{code}

Here are some `RussianDoll`s:

\begin{code}
d1 = EmptyDoll
d2 = RussianDoll "privyet" EmptyDoll
d3 = RussianDoll "matry" (RussianDoll "osh" (RussianDoll "ka" EmptyDoll))
d4 = RussianDoll "and on and on" d4
\end{code}


Write a function to return the message in the innermost non-empty doll:

> innerMostMessage :: RussianDoll -> String
> innerMostMessage EmptyDoll = error "empty doll!"
> innerMostMessage (RussianDoll m EmptyDoll) = m --returns message of RussianDoll
> innerMostMessage (RussianDoll _ d) = innerMostMessage d

Write a function to reverse the order of messages in a doll:

\begin{code}
reverseMessages :: RussianDoll -> RussianDoll
reverseMessages d = rev d EmptyDoll
  where rev EmptyDoll r = r
        rev (RussianDoll m d) r = rev d (RussianDoll m r)
\end{code}


Polymorphic Types
-----------------

A polymorphic type is a type defined using one or more type variables.

E.g., here is a box type parameterized by a single type variable:
-- takes an a and returns a Ubox of type a
> data UniversalBox a = UBox a deriving Show

The type name, `UniversalBox`, is now a *type constructor*. We must provide it
with a data type `T` to "specialize" it as `UniversalBox T`, which has a value
constructor `UBox` that takes a value of type `T`.

E.g., here are some different `UniversalBox` values:

\begin{code}
ub1 :: UniversalBox Bool
ub1 = UBox True

ub2 :: UniversalBox [Int]
ub2 = UBox [1..10]

ub4 :: Num a => UniversalBox (a -> a)
ub4 = UBox (\x -> 2*x)
\end{code}


E.g., let's define some some functions on `UniversalBox` values:

\begin{code}
catBoxes :: UniversalBox [a] -> UniversalBox [a] -> UniversalBox [a]
catBoxes (UBox l1) (UBox l2) = UBox $ l1 ++ l2


sumBoxes :: Num a => [UniversalBox a] -> UniversalBox a
sumBoxes [] = UBox 0
sumBoxes (UBox n : bs) = let UBox ns = sumBoxes bs 
                           in UBox $ n + ns
\end{code}


We say that the `UniversalBox` type constructor has "kind" (* -> *), where *
denotes a monomorphic type. I.e., the type constructor takes a monomorphic type
and produces a monomorphic type. Note that all values have types of kind *. 

---

A polymorphic type defined in Prelude is `Maybe`, defined as:

\begin{code}
data Maybe a = Just a | Nothing deriving Show
\end{code}

We use `Maybe` to create types that can represent both a value or the absence of
a value. This allows us to write functions with well-defined types that can
return values that represent "failed" computations (without a magical "null"
return value or by raising an exception, as might be done in other languages).

E.g., consider:

Maybe and Just are used here for error handling. The function does not always return a tuple of two doubles, sometimes it returns Nothing.

> quadRoots :: Double -> Double -> Double -> Maybe (Double,Double)
> quadRoots a b c = let d = b^2-4*a*c
>                       sd = sqrt d
>                   in if d < 0
>                      then Nothing
>                      else Just ((-b+sd)/(2*a), (-b-sd)/(2*a))
> 
> find :: (a -> Bool) -> [a] -> Maybe a
> find _ [] = Nothing
> find p (x:xs) | p x = Just x
>               | otherwise = find p xs
>
> idLookup :: Integer -> String
> idLookup id = case find ((== id) . fst) db of
>                 Nothing -> "Not found"
>                 Just (_, name) -> name
>   where db = [(1234, "John Doe"), 
>               (2345, "Jane Doe"), 
>               (3456, "Mary Doe")]

---

Another polymorphic type found in Prelude is `Either`, defined as:

\begin{code}
data Either a b = Left a | Right b deriving Show
\end{code}

We often use `Either` to create data types where the `Left` constructor
contains error values, and the `Right` constructor contains correct values.

Right is right! 

> find' :: (a -> Bool) -> [a] -> Either String a
> find' _ [] = Left "List was empty"
> find' p (x:xs) | p x = Right x
>                | null xs = Left "No element satisifying predicate"
>                | otherwise = find' p xs

Note that the `Either` type has kind (* -> * -> *), as its type constructor
takes two data types as parameters to fully specialize it.


What is the kind of type `T`? (try using :kind and :t)

A type with the above kind is:
parenthesis means (a b) is concrete type and a is itself a type constructor


> data T a b = T (a b)


---

The built-in list is just another polymorphic type! We can define our own list
type like this:


Use :- as value constructor 
Functions exactly as :
\begin{code}
infixr 5 :- 
data List a = a :- (List a) | Null deriving Show

\end{code}

> data List a = Cons a (List a) | Empty deriving Show

Here are some lists:

\begin{code}
l1 :: List Char
l1 = 'h' :- 'e' :- 'l' :- 'l' :- 'o' :- Null

l2 :: List Char
l2 = 'h' :- 'a' :- Null

l3 :: List (List Int)
l3 = (1 :- 2 :- Null) :- (3 :- 4 :- Null) :- Null
\end{code}


Let's define some list functions!

\begin{code}
enumFromToL :: (Eq a, Enum a) => a -> a -> List a
enumFromToL m n | m == n = m :- Null
                | otherwise = m :- enumFromToL (succ m) n


enumFromL :: (Eq a, Enum a) => a -> List a
enumFromL m = m :- enumFromL (succ m)


takeL :: Int -> List a -> List a
takeL 0 _    = Null
takeL _ Null = Null
takeL n (x :- xs) = x :- takeL (n-1) xs


mapL :: (a -> b) -> List a -> List b
mapL _ Null = Null
mapL f (x :- xs) = f x :- mapL f xs
\end{code}


Type Classes
------------

A type class defines a collection of functions associated with conforming types.

Types that conform to a type class are called *instances* of that class, and the
functions defined by the class are called *methods*.

Here's a silly class that defines two methods:
a is Explosive when we can call method explode on a 

\begin{code}
class Explosive a where
  explode :: a -> [a]
\end{code}

The above says that for all types `a` that are instances of `Explosive`, there
is an `explode` function that takes that type and returns a list of that type. 

To make a type an instance of a class, we need to declare it so and implement
the requisite method(s):

:info Int should show Ints are now Explosive 

> instance Explosive Integer where
>   explode n = [n..n+10]
>
> instance Explosive Char where
>   explode c = take 10 $ enumFrom c
>
> instance Explosive [a] where
>   explode = map (:[])

Now we can apply `explode` to `Integer`, `Char`, and list values. Looking at the
output of ":t explode" in GHCi confirms that `explode` has the constrained type:

    explode :: Explosive a => a -> [a]

We can also use the class as a constraint in other functions:

\begin{code}
blowItAllUp :: Explosive a => [a] -> [[a]]
blowItAllUp = map explode
\end{code}

---

Consider the class `Eq`, defined thusly in Prelude:

\begin{verbatim}
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x /= y)
  
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
\end{verbatim}

Note that it also contains definitions for its methods --- `==` and `/=` are
defined in terms of each other. This makes it so that an instance need only
define one of the methods; the missing method will default to the implementation
found in the class.

E.g., make the `Student` type defined earlier (show below) an instance of `Eq`:

    data Student = Student {
      firstName :: String,
      lastName  :: String,
      studentId :: Integer,
      grades    :: [Char]
    } 

\begin{code}
instance Eq Student where
  (==) :: Student -> Student -> Bool
  (Student _ _ id1 _) == (Student _ _ id2 _) = id1 == id2
\end{code}

---

Here is the definition of class `Ord`, also from Prelude:

\begin{verbatim}
class (Eq a) => Ord a where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
\end{verbatim}

The `(Eq a) => Ord a` means that `Ord` is a subclass of `Eq`; i.e., that `Ord`
inherits all the methods of `Eq`, and that all instances of `Ord` must also
be instances of `Eq`.

Because of all the method implementations found in `Ord`, a minimal instance
need only supply either the `compare` or `<=` methods. 

E.g., make `Student` an instance of `Ord`:

\begin{code}
instance Ord Student where
  compare :: Student -> Student -> Ordering
  compare (Student _ _ id1 _) (Student _ _ id2 _) = compare id1 id2
\end{code}

Making a polymorphic type an instance of a class may require adding constraints
to the instance declaration. E.g., complete our `List` instance of `Eq`:

\begin{code}
instance (Eq a) => Eq (List a) where
  (==) :: Eq a => List a -> List a -> Bool
  Null == Null = True
  (x :- xs) == (y :- ys) = x == y && xs == ys
  _ == Null = False
  Null == _ = False
\end{code}

---

Another useful class is `Foldable`, which at minimum requires a single be 
defined, `foldr`:

\begin{verbatim}
class Foldable t where
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldl   :: (b -> a -> b) -> b -> t a -> b
  foldr1  :: (a -> a -> a) -> t a -> a
  foldl1  :: (a -> a -> a) -> t a -> a
  null    :: t a -> Bool
  length  :: t a -> Int
  elem    :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum     :: Num a => t a -> a
  product :: Num a => t a -> a
\end{verbatim}


Make `List` an instance of `Foldable`:

\begin{code}
instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ v Null = v
  foldr f v (x :- xs) = f x $ foldr f v xs
\end{code}


-- Automatic derivation

The Haskell compiler can automagically derive instances of the classes Eq, Ord,
Enum, Bounded, Show, and Read for user defined data types (which meet certain
criteria) by using the `deriving` clause. This makes it easier to compare,
enumerate, print/parse data when the default behavior is sufficient.

E.g.,

\begin{code}
data Suit = Diamond | Club | Heart | Spade 
            deriving (Eq, Ord, Enum, Bounded, Show, Read)
\end{code}
