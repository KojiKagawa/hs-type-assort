{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module TwistedTurtle where

import Data.STRef

import Turtle
import ColorTurtle
import Turtle3D
import Assort(CastClass( CastFrom ))

newtype TwistedTurtle s e = TwistedTurtle (STRef s e)
class FromTwistedTurtle s a | a -> s where
     fromTwistedTurtle :: TwistedTurtle s a -> a
{-# ANN type TwistedTurtle (CastFrom ''FromTwistedTurtle ''TwistedTurtle) #-}

mkTwistedTurtle :: FromTwistedTurtle s a => STRef s a -> a
mkTwistedTurtle r = fromTwistedTurtle (TwistedTurtle r)

instance Movable s self => Movable s (TwistedTurtle s self) where
    forward (TwistedTurtle r) d = readSTRef r >>= \ t -> forward t d
    turn (TwistedTurtle r) a  = readSTRef r >>= \ t -> turn t (-a)

instance HasColor s self => HasColor s (TwistedTurtle s self) where
    getColor (TwistedTurtle r) = readSTRef r >>= getColor
    setColor (TwistedTurtle r) c = readSTRef r >>= \ t -> setColor t c

instance Movable3D s self =>
         Movable3D s (TwistedTurtle s self) where
    bank (TwistedTurtle r) a  = readSTRef r >>= \ t -> bank t (-a)
    pitch (TwistedTurtle r) a = readSTRef r >>= \ t -> pitch t (-a)


