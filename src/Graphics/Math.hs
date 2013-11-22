{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Math
    ( module Data.Vect.Double
    , module Data.Vect.Double.Instances

    , AlmostEq (..)

    , tau
    , Scalar
    , BasicVector (..)
    , xAxis, yAxis, zAxis --, wAxis
    , vone
    , dot, cross
) where

import Data.List (sort)
import Data.Function (on)
import Data.Vect.Double
import Data.Vect.Double.Instances
import Debug.Trace

deriving instance Ord Vec3

dot :: DotProd v => v -> v -> Scalar
dot = dotprod

cross :: CrossProd v => v -> v -> v
cross = crossprod

----------------------------------------------------------------------

epsilon = 1e-6
class AlmostEq a where
    (~=) :: a -> a -> Bool

tracePair :: (Show a, Show b) => (a -> b -> c) -> a -> b -> c
tracePair f a b = trace (show a ++ " " ++ show b ++ "\n") $ f a b

instance (Show a, Ord a, AlmostEq a) => AlmostEq [a] where
    --(~=) a b = on (==) length a b && and (on (zipWith (~=)) sort a b)
    (~=) a b = on (==) length a b && and (zipWith (~=) a b)
instance AlmostEq Double where
    a ~= b = let d = a - b in d < epsilon && d > -epsilon
instance AlmostEq Vec2 where
    (~=) = on (~=) vunpack
instance AlmostEq Vec3 where
    (~=) = on (~=) vunpack

----------------------------------------------------------------------

type Scalar = Double

tau :: Floating a => a
tau = 2*pi

class BasicVector v where
   vmap     :: (Scalar -> Scalar) -> v -> v
   vzip     :: (Scalar -> Scalar -> Scalar) -> v -> v -> v
   vfold    :: (Scalar -> Scalar -> Scalar) -> v -> Scalar
   vpack    :: [Scalar] -> Maybe v
   vunpack  :: v -> [Scalar]
   vpromote :: Scalar -> v

{-
instance BasicVector Scalar where
    vmap f x = f x
    vzip f x y = f x y
    vfold f x = x
    vpack (x:_) = Just x
    vpack _ = Nothing
    vunpack x = [x]
    vpromote x = x
-}

instance BasicVector Vec2 where
    vmap f (Vec2 x y) = Vec2 (f x) (f y)
    vzip f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (f x1 x2) (f y1 y2)
    vfold f (Vec2 x y) = f x y
    vpack (x:y:_) = Just $ Vec2 x y
    vpack _ = Nothing
    vunpack (Vec2 x y) = [x,y]
    vpromote x = Vec2 x x

instance BasicVector Vec3 where
    vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
    vzip f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
        Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
    vfold f (Vec3 x y z) = f x (f y z)
    vpack (x:y:z:_) = Just $ Vec3 x y z
    vpack _ = Nothing
    vunpack (Vec3 x y z) = [x,y,z]
    vpromote x = Vec3 x x x

instance BasicVector Vec4 where
    vmap f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)
    vzip f (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) =
        Vec4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
    vfold f (Vec4 x y z w) = f (f x y) (f z w)
    vpack (x:y:z:w:_) = Just $ Vec4 x y z w
    vpack _ = Nothing
    vunpack (Vec4 x y z w) = [x,y,z,w]
    vpromote x = Vec4 x x x x

----------------------------------------------------------------------

xAxis :: Vec3
xAxis = Vec3 1 0 0

yAxis :: Vec3
yAxis = Vec3 0 1 0

zAxis :: Vec3
zAxis = Vec3 0 0 1

vone :: BasicVector v => v
vone = vpromote 1

