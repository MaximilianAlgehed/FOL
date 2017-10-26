{-# LANGUAGE FlexibleInstances #-}
module FOL where

import Data.List

data Term = Fun String [Term]
          | Var String

instance Show Term where
  show (Fun n ts) = n ++ arguments ts
  show (Var s)    = s

arguments :: [Term] -> String
arguments [] = ""
arguments ts = "(" ++ intercalate "," (map show ts) ++ ")"

{- Utility for constructing functions -}
constant :: String -> Term
constant s = Fun s []

unaryFunction :: String -> Term -> Term
unaryFunction s t = Fun s [t]

binaryFunction :: String -> Term -> Term -> Term
binaryFunction s t u = Fun s [t, u]

ternaryFunction :: String -> Term -> Term -> Term -> Term
ternaryFunction s t u v = Fun s [t, u, v]

data Prop = A (Term -> Prop)
          | E (Term -> Prop)
          | Term :==: Term
          | Prop :&&: Prop
          | Prop :||: Prop
          | Not Prop
          | Pred String [Term]

tptp :: [Prop] -> String
tptp ps = unlines $ [ "fof(axm" ++ show i ++ ",axiom," ++ show p ++ ")."
                    | (i, p) <- zip [0..] ps ]

{- Utility for constructing predicates -}
atomic :: String -> Prop 
atomic p = Pred p []

unaryPredicate :: String -> Term -> Prop
unaryPredicate n t = Pred n [t]

binaryPredicate :: String -> Term -> Term -> Prop
binaryPredicate n t u = Pred n [t, u]

ternaryPredicate :: String -> Term -> Term -> Term -> Prop
ternaryPredicate n t u v = Pred n [t, u, v]

{- Constructing propositions -} 
class Binder a where
  bind :: ((Term -> Prop) -> Prop) -> a -> Prop

instance Binder (Term -> Prop) where
  bind = ($)

instance {-# OVERLAPPABLE #-} Binder p => Binder (Term -> p) where
  bind b p = b $ \x -> bind b $ p x

forall, exists :: Binder a => a -> Prop
forall = bind A
exists = bind E

(===) :: Term -> Term -> Prop
(===) = (:==:)

(&.) :: Prop -> Prop -> Prop
(&.) = (:&&:)

(|.) :: Prop -> Prop -> Prop
(|.) = (:||:)

not' :: Prop -> Prop
not' = Not

{- Derived combinators -}
(==>) :: Prop -> Prop -> Prop
p ==> q = not' p |. q

(<==>) :: Prop -> Prop -> Prop
p <==> q = (p ==> q) &. (q ==> p)

{- First order representation -}
data FOPropRep = All String FOPropRep
               | Exi String FOPropRep
               | Eql Term Term
               | Imp FOPropRep FOPropRep
               | And FOPropRep FOPropRep
               | Or  FOPropRep FOPropRep
               | Neg FOPropRep
               | Pre String [Term]

gensym :: Int -> String
gensym n
  | n < 26    = letter n
  | otherwise = gensym (n `div` 26) ++ letter (n `mod` 26)
  where
    letter :: Int -> String
    letter n = (:[]) $ ['A'..'Z'] !! n

toFORep :: Int -> Prop -> FOPropRep
toFORep nv p = case p of
  A body    -> let name = gensym nv in All name $ toFORep (nv + 1) (body (Var name))
  E body    -> let name = gensym nv in Exi name $ toFORep (nv + 1) (body (Var name))
  a :==: b  -> Eql a b
  p :&&: q  -> And (toFORep nv p) (toFORep nv q)
  p :||: q  -> Or  (toFORep nv p) (toFORep nv q)
  Not p     -> Neg (toFORep nv p)
  Pred n ts -> Pre n ts

instance Show Prop where
  show = show . toFORep 0

instance Show FOPropRep where
  showsPrec d rep = case rep of
    All n p  -> showParen (d > 0) . showString $ "! [" ++ n ++ "] : " ++ show p
    Exi n p  -> showParen (d > 0) . showString $ "? [" ++ n ++ "] : " ++ show p
    Eql a b  -> showString $ show a ++ " = " ++ show b
    And p q  -> showParen (d > 2) $ showsPrec 2 p . showString " & " . showsPrec 3 q
    Or  p q  -> showParen (d > 1) $ showsPrec 1 p . showString " | " . showsPrec 2 q
    Neg p    -> showParen (d > 3) $ showString "~" . showsPrec 3 p
    Pre p ts -> showString $ p ++ arguments ts
