{-# LANGUAGE TemplateHaskellQuotes #-}

module Times where

import Carrefour(CastClass( CastFrom ) )
import Exp

data Times s1 s2 = Times s1 s2

class FromTimes a where
    fromTimes :: Times a a -> a
{-# ANN type Times (CastFrom ''FromTimes ''Times) #-}

instance  (Eval s1, Eval s2) => Eval (Times s1 s2) where
   eval (Times s1 s2) = eval s1 * eval s2