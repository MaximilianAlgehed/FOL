module TheoryHelpers where

import FOL

associative :: (Term -> Term -> Term) -> Prop
associative f = forall $ \x y z -> f x (f y z) === f (f x y) z

commutative :: (Term -> Term -> Term) -> Prop
commutative f = forall $ \x y -> f x y === f y x

reflexive :: (Term -> Term -> Prop) -> Prop
reflexive p = forall $ \x -> p x x

transitive :: (Term -> Term -> Prop) -> Prop
transitive p = forall $ \x y z -> p x y &. p y z ==> p x z
