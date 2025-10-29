{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Turtle where

import Control.Monad.ST
import Data.STRef
import Carrefour(CastClass( CastFrom ))

data Turtle s = Turtle {x:: STRef s Double, y:: STRef s Double, t:: STRef s Double}

class FromTurtle s a | a -> s where
     fromTurtle :: Turtle s -> a
{-# ANN type Turtle (CastFrom ''FromTurtle ''Turtle) #-}

class TurtleLike a s | a -> s  where
   forward :: a -> Double -> ST s ()
   turn    :: a -> Double -> ST s ()

-- renmei class TurtleLike a s :: AllTurtle s

rotate t (x, y) =
    let c = cos t; s = sin t in
         (x * c - y * s, x * s + y *c)

instance TurtleLike (Turtle s) s where
   forward (Turtle {x, y, t}) distance = do
        x0 <- readSTRef x
        y0 <- readSTRef y
        t0 <- readSTRef t
        let (x1, y1) = rotate t0 (x0, y0)
        writeSTRef x x1
        writeSTRef y y1
   turn (Turtle {x, y, t}) dt = do
        t0 <- readSTRef t
        writeSTRef t (t0 + dt) 
        