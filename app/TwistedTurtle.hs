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
import Carrefour(CastClass( CastFrom ))

newtype TwistedTurtle s e = TwistedTurtle (STRef s e)
class FromTwistedTurtle s a | a -> s where
     fromTwistedTurtle :: TwistedTurtle s a -> a
{-# ANN type TwistedTurtle (CastFrom ''FromTwistedTurtle ''TwistedTurtle) #-}

twistedTurtle :: FromTwistedTurtle s a => STRef s a -> a
twistedTurtle r = fromTwistedTurtle (TwistedTurtle r)

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


