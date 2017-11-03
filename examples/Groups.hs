module Groups where

import FOL
import TheoryHelpers
import Backend

(*.) :: Term -> Term -> Term
(*.) = binaryFunction "op"

e :: Term
e = constant "e"

group :: [Prop]
group = [ associative (*.)
        , forall $ \x -> e *. x === x
        , forall $ \x -> x *. e === x
        , forall $ \x -> exists $ \x_inv -> (x *. x_inv === e) &. (x_inv *. x === e) ]

uniqueness :: Prop
uniqueness = forall $ \x e' -> x *. e' === x ==> e' === e
