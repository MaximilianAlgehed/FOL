module TheoryHelpers where

import FOL

associative :: (Term -> Term -> Term) -> Prop
associative f = forall $ \x y z -> f x (f y z) === f (f x y) z

commutative :: (Term -> Term -> Term) -> Prop
commutative f = forall $ \x y -> f x y === f y x
