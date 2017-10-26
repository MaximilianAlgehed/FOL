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

symmetric :: (Term -> Term -> Prop) -> Prop
symmetric p = forall $ \x y -> p x y <==> p y x

antisymmetric :: (Term -> Term -> Prop) -> Prop
antisymmetric p = forall $ \x y -> p x y &. p y x ==> x === y

equivalence :: (Term -> Term -> Prop) -> Prop
equivalence p =  transitive p
              &. symmetric  p
              &. reflexive  p

partialOrder :: (Term -> Term -> Prop) -> Prop
partialOrder p =  reflexive  p
               &. transitive p
               &. antisymmetric p

totalOrder :: (Term -> Term -> Prop) -> Prop
totalOrder p =  partialOrder p
             &. forall (\x y -> p x y |. p y x)
