{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TwistedTurtle where

import Turtle
import ColorTurtle
import Turtle3D

newtype TwistedTurtle self=
     TwistedTurtle self

instance TurtleLike self s => TurtleLike (TwistedTurtle self) s where
    forward (TwistedTurtle t) = forward t
    turn (TwistedTurtle t) a    = turn t (-a)

instance HasColor self s => HasColor (TwistedTurtle self) s where
    getColor (TwistedTurtle t) = getColor t
    setColor (TwistedTurtle t) = setColor t 

instance Turtle3DLike self s =>
         Turtle3DLike (TwistedTurtle self) s where
    bank (TwistedTurtle t) a  = bank t (-a)
    pitch (TwistedTurtle t) a = pitch t (-a)


