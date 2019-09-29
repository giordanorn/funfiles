module Coproduct where

import Prelude hiding (Either(..))

data Either a b = Left a
                | Right b
                deriving (Show , Eq)

h :: Either a b -> d
h = undefined
    where 
        f :: a -> d
        f = error "f dada"
        g :: b -> d
        g = error "g dada"
