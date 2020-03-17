module TheoryHelpers where

import FOL

associative :: (Term -> Term -> Term) -> Prop
associative f = forall $ \x y z -> f x (f y z) === f (f x y) z

commutative :: (Term -> Term -> Term) -> Prop
commutative f = forall $ \x y -> f x y === f y x

reflexive :: (Term -> Term -> Prop) -> Prop
reflexive p = forall $ \x -> p x x

irreflexive :: (Term -> Term -> Prop) -> Prop
irreflexive p = forall $ \x -> not' $ p x x

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

total :: (Term -> Term -> Prop) -> Prop
total p = forall $ \x y -> not' (x === y) ==> (p x y |. p y x)

totalOrder :: (Term -> Term -> Prop) -> Prop
totalOrder p =  partialOrder p
             &. total p
