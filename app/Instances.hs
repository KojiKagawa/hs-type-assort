{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instances where

import Turtle
import ColorTurtle
import Turtle3D
import TwistedTurtle

instance HasColor (Turtle s) s where
   getColor _ = return 0
   setColor _ _ = return ()

instance HasColor (Turtle3D s) s where
   getColor _ = return 0
   setColor _ _ = return ()

instance Turtle3DLike (Turtle s) s where
    bank _ _ = return ()
    pitch _ _ = return ()

instance Turtle3DLike (ColorTurtle s) s where
    bank _ _ = return ()
    pitch _ _ = return ()