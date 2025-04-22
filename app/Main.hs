{-# LANGUAGE QuasiQuotes #-}

module Main where

import Carrefour( carrefour )

import Turtle
import ColorTurtle
import Turtle3D
import TwistedTurtle
import Instances

[carrefour| 
data AllTurtle s <- Turtle s | ColorTurtle s | Turtle3D s | TwistedTurtle _Self 
    deriving (TurtleLike _Self s, HasColor _Self s) 
|]

main :: IO ()
main = do
  -- putStrLn [carrefour| x |]
  putStrLn "Hello, Haskell!"

