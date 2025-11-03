{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module ColorTurtle where

import Control.Monad.ST
import Data.STRef
import Turtle
import Assort(CastClass( CastFrom ))

data ColorTurtle s = ColorTurtle {x:: STRef s Double, y:: STRef s Double, t:: STRef s Double, c:: STRef s Int}

class FromColorTurtle s a | a -> s where
     fromColorTurtle :: ColorTurtle s -> a
{-# ANN type ColorTurtle (CastFrom ''FromColorTurtle ''ColorTurtle) #-}

mkColorTurtle x y t c = fromColorTurtle $ ColorTurtle x y t c

instance Movable s (ColorTurtle s) where
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

class HasColor s a | a -> s where
   getColor :: a -> ST s Int
   setColor :: a -> Int -> ST s ()

instance HasColor s (ColorTurtle s) where
   getColor (ColorTurtle {c}) = do
      readSTRef c
   setColor (ColorTurtle {c}) c1 = do
      writeSTRef c c1