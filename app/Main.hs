{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Carrefour( carrefour, Cast, cast)

import Turtle
import ColorTurtle
import Turtle3D
import TwistedTurtle
import Instances

import Control.Monad.ST
import Data.STRef
import Data.Either

[carrefour| 
data AllTurtle s <- Turtle s | ColorTurtle s | Turtle3D s | TwistedTurtle _Self 
    deriving (TurtleLike _Self s, HasColor _Self s) 
|]

{-
data AllTurtle s
    = AllTurtle1 (Turtle.Turtle s)
    | AllTurtle2 (ColorTurtle.ColorTurtle s)
    | AllTurtle3 (Turtle3D.Turtle3D s)
    | AllTurtle4 (TwistedTurtle.TwistedTurtle (AllTurtle s))

class ToAllTurtle _self s | _self -> s
    where {toAllTurtle :: _self -> AllTurtle s}
instance (ToAllTurtle a s,
          ToAllTurtle b s) => ToAllTurtle (Data.Either.Either a b) s
    where {toAllTurtle = Data.Either.either toAllTurtle toAllTurtle}
instance ToAllTurtle a s => Carrefour.Cast a (AllTurtle s)
    where {cast _self = toAllTurtle _self}
instance ToAllTurtle (Turtle.Turtle s) s
    where {toAllTurtle = AllTurtle1}
instance ToAllTurtle (ColorTurtle.ColorTurtle s) s
    where {toAllTurtle = AllTurtle2}
instance ToAllTurtle (Turtle3D.Turtle3D s) s
    where {toAllTurtle = AllTurtle3}
instance ToAllTurtle (TwistedTurtle.TwistedTurtle (AllTurtle s)) s
    where {toAllTurtle = AllTurtle4}
class FromAllTurtle _self s | _self -> s
    where {fromAllTurtle :: AllTurtle s -> Maybe _self}
instance FromAllTurtle (Turtle.Turtle s) s
    where fromAllTurtle (AllTurtle1 x) = Just x
          fromAllTurtle _ = Nothing
instance FromAllTurtle (ColorTurtle.ColorTurtle s) s
    where fromAllTurtle (AllTurtle2 x) = Just x
          fromAllTurtle _ = Nothing
instance FromAllTurtle (Turtle3D.Turtle3D s) s
    where fromAllTurtle (AllTurtle3 x) = Just x
          fromAllTurtle _ = Nothing
instance FromAllTurtle (TwistedTurtle.TwistedTurtle (AllTurtle s))
                       s
    where fromAllTurtle (AllTurtle4 x) = Just x
          fromAllTurtle _ = Nothing

---- defineInstance
instance Turtle.TurtleLike (AllTurtle s) s
    where {forward (AllTurtle1 x1) = forward x1;
           forward (AllTurtle2 x1) = forward x1;
           forward (AllTurtle3 x1) = forward x1;
           forward (AllTurtle4 x1) = forward x1;
           turn (AllTurtle1 x1) = turn x1;
           turn (AllTurtle2 x1) = turn x1;
           turn (AllTurtle3 x1) = turn x1;
           turn (AllTurtle4 x1) = turn x1}
instance ColorTurtle.HasColor (AllTurtle s) s
    where {getColor (AllTurtle1 x1) = getColor x1;
           getColor (AllTurtle2 x1) = getColor x1;
           getColor (AllTurtle3 x1) = getColor x1;
           getColor (AllTurtle4 x1) = getColor x1;
           setColor (AllTurtle1 x1) = setColor x1;
           setColor (AllTurtle2 x1) = setColor x1;
           setColor (AllTurtle3 x1) = setColor x1;}
-}

main = stToIO (do
  -- putStrLn [carrefour| x |]
  x1 <- newSTRef 0.0
  y1 <- newSTRef 1.0
  t1 <- newSTRef pi
  let turtle1 = Turtle x1 y1 t1
  x2 <- newSTRef (-1.0)
  y2 <- newSTRef 0.0
  t2 <- newSTRef 0.0
  c2 <- newSTRef 0x00ff0000
  let turtle2 = ColorTurtle x2 y2 t2 c2
--      turtle3 :: Cast (ColorTurtle RealWorld) self0 => TwistedTurtle self0
      turtle3 = TwistedTurtle ((id :: AllTurtle s -> AllTurtle s) (cast turtle2))
      turtles = (id :: [AllTurtle s] -> [AllTurtle s]) [cast turtle1, cast turtle2, cast turtle3]
  mapM_ (\ t -> forward t 3.3) turtles)

{-
  want to deduce
      self0 = AllTurtle RealWorld
  from   
      (ToAllTurtle (TwistedTurtle self0) RealWorld, (Cast (ColorTurtle RealWorld) self0))
-}