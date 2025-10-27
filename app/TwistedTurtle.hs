{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TwistedTurtle where

import Data.STRef

import Turtle
import ColorTurtle
import Turtle3D

newtype TwistedTurtle s self = TwistedTurtle (STRef s self)

instance TurtleLike self s => TurtleLike (TwistedTurtle s self) s where
    forward (TwistedTurtle r) d = readSTRef r >>= \ t -> forward t d
    turn (TwistedTurtle r) a  = readSTRef r >>= \ t -> turn t (-a)

instance HasColor self s => HasColor (TwistedTurtle s self) s where
    getColor (TwistedTurtle r) = readSTRef r >>= getColor 
    setColor (TwistedTurtle r) c = readSTRef r >>= \ t -> setColor t c

instance Turtle3DLike self s =>
         Turtle3DLike (TwistedTurtle s self) s where
    bank (TwistedTurtle r) a  = readSTRef r >>= \ t -> bank t (-a)
    pitch (TwistedTurtle r) a = readSTRef r >>= \ t -> pitch t (-a)


