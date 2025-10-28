{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fplugin DefaultingPlugin #-}

module Main where

import Carrefour( carrefour, Cast, ucast, dcast)

import Turtle
import ColorTurtle
import Turtle3D
import TwistedTurtle
import Instances

import Exp
import PPrint
import Times
import TimesPPrint
import PickExp

import Control.Monad.ST
import Data.STRef
import Data.Either

[carrefour| 
data assorted AllTurtle s = Turtle s | ColorTurtle s | Turtle3D s | TwistedTurtle s _Self 
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
    where {ucast _self = toAllTurtle _self}
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

main1 = do
  p <-  stToIO (do
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
    r2 <- newSTRef (ucast turtle2)
    let
      turtle3 = TwistedTurtle ((id :: STRef s (AllTurtle s) -> STRef s (AllTurtle s)) r2)
      turtles = (id :: [AllTurtle s] -> [AllTurtle s]) [ucast turtle1, ucast turtle2, ucast turtle3]
    mapM_ (\ t -> forward t 3.3) turtles
    x2' <- readSTRef x2
    y2' <- readSTRef y2
    return (x2', y2'))
  print p


{-
  want to deduce
      self0 = AllTurtle RealWorld
  from   
      (ToAllTurtle (TwistedTurtle self0) RealWorld, (Cast (ColorTurtle RealWorld) self0))
-}

[carrefour| 
data assorted AllExp = Lit | Plus _Self _Self | Times _Self _Self
    deriving (Eval _Self, PPrint _Self, Pick0 _Self _Self _Self) 
|]

{-
-- 以下の書き方でも可
[carrefour| 
data assorted AllExp = Lit | Plus AllExp AllExp | Times AllExp AllExp
    deriving (Eval AllExp, PPrint AllExp, Pick0 AllExp AllExp _Self) 
|]
-}

main2 = let -- ucast = toAllExp
            -- ucast' = toAllExp
-- ucast' = ucast にすると 
-- Cast (Plus AllExp AllExp) a, Cast (Times AllExp AllExp) a, Cast Lit a
-- が WantedConstraints に含まれなくなる
-- Todo: 上記の原因を調査する
-- Todo: ucast ではなく Source を固定した castLit, castPlus などを定義して
--       それらを使うようにする
            -- cast' = ucast
            -- exps :: [ AllExp ]
            -- exps = [ ucast (Lit 1.2)
            --        , ucast (Plus (cast' (Lit 3.4)) (cast' (Lit 7.8)))
            --        , ucast (Times (cast' (Lit 0.6)) (cast' (Plus (cast' (Lit (- 1.2))) (cast' (Lit 3.09))))) ]
            exps = [ fromLit (Lit 1.2)
                   , fromPlus (Plus (fromLit (Lit 3.4)) (fromLit (Lit 7.8)))
                   , fromTimes (Times (fromLit (Lit 0.6)) (fromPlus (Plus (fromLit (Lit (- 1.2))) (fromLit (Lit 3.09))))) ]
            output =  concat $ map pprint exps
          in print output

-- foo a = let x = read "False"
--           in show [(x, a), (x, read "True")]
-- main3 = putStrLn $ foo False

main = do main1
          putStrLn "-----"
          main2
          putStrLn "-----"
          -- main3