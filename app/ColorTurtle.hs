{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ColorTurtle where

import Control.Monad.ST
import Data.STRef
import Turtle

data ColorTurtle s = ColorTurtle {x:: STRef s Double, y:: STRef s Double, t:: STRef s Double, c:: STRef s Int}

instance TurtleLike (ColorTurtle s) s where
   forward (ColorTurtle {x, y, t}) distance = do
        x0 <- readSTRef x
        y0 <- readSTRef y
        t0 <- readSTRef t
        let (x1, y1) = rotate t0 (x0, y0)
        writeSTRef x x1
        writeSTRef y y1
   turn (ColorTurtle {x, y, t}) dt = do
        t0 <- readSTRef t
        writeSTRef t (t0 + dt) 

class HasColor a s | a -> s where
   getColor :: a -> ST s Int
   setColor :: a -> Int -> ST s ()

instance HasColor (ColorTurtle s) s where
   getColor (ColorTurtle {c}) = do
      readSTRef c
   setColor (ColorTurtle {c}) c1 = do
      writeSTRef c c1