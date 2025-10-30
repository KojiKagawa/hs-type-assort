{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instances where

import Turtle
import ColorTurtle
import Turtle3D
import TwistedTurtle

instance HasColor s (Turtle s) where
   getColor _ = return 0
   setColor _ _ = return ()

instance HasColor s (Turtle3D s) where
   getColor _ = return 0
   setColor _ _ = return ()

instance Movable3D s (Turtle s) where
    bank _ _ = return ()
    pitch _ _ = return ()

instance Movable3D s (ColorTurtle s) where
    bank _ _ = return ()
    pitch _ _ = return ()