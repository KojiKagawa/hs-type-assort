{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Turtle3D where 

import Control.Monad.ST
import Data.STRef
import Turtle
import Carrefour(CastClass( CastFrom ))

type V3D = (Double, Double, Double)

data Turtle3D s = Turtle3D {
       p :: STRef s V3D,
       v:: STRef s V3D,
       r:: STRef s V3D,
       u:: STRef s V3D
     }

class FromTurtle3D s a | a -> s where
     fromTurtle3D :: Turtle3D s -> a
{-# ANN type Turtle3D (CastFrom ''FromTurtle3D ''Turtle3D) #-}

class Movable3D s a | a -> s where
   bank  :: a -> Double -> ST s ()
   pitch :: a -> Double -> ST s ()

-- 左手系の3D座標系で、y軸が上向き（x軸が右向き、z軸が奥向き）
__direction = (1, 0, 0)
__right     = (0, -1, 0)
__up        = (0, 0, -1)

scale3D (x, y, z) s = (s * x, s * y, s * z)
add3D (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
sub3D (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
cross3D (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)
dot3D (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2
norm3D v = sqrt (dot3D v v)
normalize3D v = let n = norm3D v in scale3D v (recip n)
rotate3D v axis a =
    let inner  = dot3D v axis      -- inner product
        axis1  = scale3D axis inner
        v1     = sub3D v axis1
        proj90 = cross3D axis v1
        v2     = scale3D v1 (cos a)
        v3     = scale3D proj90 (sin a)
        v4     = add3D v3 proj90
        v5     = add3D v4 axis1
        in v5
rotate3D360 v axis deg = rotate3D v axis (deg / 180 * pi)

instance Movable s (Turtle3D s) where
   forward (Turtle3D {p, v}) d = do
      p0 <- readSTRef p
      v0 <- readSTRef v
      let p1 = add3D p0 (scale3D v0 d)
      writeSTRef p p1
   turn (Turtle3D {p, v, r, u}) angle = do
     u0 <- readSTRef u
     d0 <- readSTRef v
     let d1 = rotate3D360 d0 u0 angle
     let d2 = normalize3D d1
     writeSTRef v d2
     r0 <- readSTRef r
     let r1 = rotate3D360 r0 u0 angle
     let r2 = normalize3D r1
     writeSTRef r r2

instance Movable3D s (Turtle3D s) where
   bank (Turtle3D {p, v, r, u}) angle = do
     d0 <- readSTRef v
     r0 <- readSTRef r
     let r1 = rotate3D360 r0 d0 angle
     let r2 = normalize3D r1
     writeSTRef r r2
     u0 <- readSTRef u
     let u1 = rotate3D360 u0 d0 angle
     let u2 = normalize3D u1
     writeSTRef u u2
   pitch (Turtle3D {p, v, r, u}) angle = do
     r0 <- readSTRef r
     d0 <- readSTRef v
     let d1 = rotate3D360 d0 r0 angle
     let d2 = normalize3D d1
     writeSTRef v d2
     u0 <- readSTRef u
     let u1 = rotate3D360 u0 r0 angle
     let u2 = normalize3D u1
     writeSTRef u u2        
   
