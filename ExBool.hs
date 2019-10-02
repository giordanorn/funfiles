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

    show True = "true"
    show False = "false"

instance Enum Bool where

    toEnum  = undefined

    fromEnum  = undefined

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) x y = ( x /|\ y ) /|\ ( x /|\ y )

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) x y = (( (x /|\ x) /|\ (y /|\ y) ) /|\ ( (x /|\ x) /|\ (y /|\ y) )) /|\ (( (x /|\ x) /|\ (y /|\ y) ) /|\ ( (x /|\ x) /|\ (y /|\ y) ))

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) False _ = True 
(/|\) _ False = True 
(/|\) True True = False 

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) x y = ((( (x /|\ x) /|\ (y /|\ y) ) /|\ ( (x /|\ x) /|\ (y /|\ y) )) /|\ (( (x /|\ x) /|\ (y /|\ y) ) /|\ ( (x /|\ x) /|\ (y /|\ y) ))) /|\ ((( (x /|\ x) /|\ (y /|\ y) ) /|\ ( (x /|\ x) /|\ (y /|\ y) )) /|\ (( (x /|\ x) /|\ (y /|\ y) ) /|\ ( (x /|\ x) /|\ (y /|\ y) )))

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) x y = (( ((( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))) /|\ ((( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )))) /|\ ( ((( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))) /|\  ((( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ))))) /|\ (( ((( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))) /|\ ((( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )))) /|\ ( ((( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))) /|\  ((( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )))))

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not x = x /|\ x

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True fst _ = fst
ifThenElse _ _ snd = snd

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) x y = (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) x y = (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ))

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) x y = ( ((( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))) /|\ ((( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )))) /|\ ( ((( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) )) /|\ (( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ) /|\ ( ((x /|\ x) /|\ (x /|\ x)) /|\ (y /|\ y) ))) /|\  ((( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) )) /|\ (( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ) /|\ ( ((y /|\ y) /|\ (y /|\ y)) /|\ (x /|\ x) ))))

infixr 1 <=>


