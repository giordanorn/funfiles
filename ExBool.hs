module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show = undefined

instance Eq Bool where

    (==) = undefined

instance Ord Bool where

    (<=) = undefined

instance Enum Bool where

    toEnum  = undefined

    fromEnum  = undefined

-- conjunction (and)
(&&) :: Bool -> Bool -> Bool
(&&) = undefined

infixr 3 &&

-- disjunction (or)
(||) :: Bool -> Bool -> Bool
(||) = undefined

infixr 2 ||

-- boolean negation
not :: Bool -> Bool
not = undefined

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse = undefined

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = undefined

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) = undefined

infixr 1 <=>


