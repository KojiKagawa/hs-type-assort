module Times where

import Exp

data Times s1 s2 = Times s1 s2

instance  (Eval s1, Eval s2) => Eval (Times s1 s2) where
   eval (Times s1 s2) = eval s1 * eval s2