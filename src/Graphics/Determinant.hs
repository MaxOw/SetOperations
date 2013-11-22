module Graphics.Determinant (det3, det4) where

import Data.Vec
import Graphics.Math (Scalar)

type Vector3 = Vec3 Scalar
type Vector4 = Vec4 Scalar
type Matrix3 = Mat33 Scalar
type Matrix4 = Mat44 Scalar

det3 :: [[Scalar]] -> Scalar
det3 m = det (matFromLists m :: Matrix3)

det4 :: [[Scalar]] -> Scalar
det4 m = det (matFromLists m :: Matrix4)
