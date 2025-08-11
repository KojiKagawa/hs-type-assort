module Exp where

data Lit = Lit Double

data Plus s1 s2 = Plus s1 s2

class Eval x where
   eval :: x -> Double

instance Eval Lit where
   eval (Lit d) = d

instance (Eval s1, Eval s2) => Eval (Plus s1 s2) where
   eval (Plus s1 s2) = eval s1 + eval s2
