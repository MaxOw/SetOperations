{-# LANGUAGE FlexibleInstances #-}
module Graphics.PolyhedraTests where

import Control.Applicative

import Test.QuickCheck
import Graphics.Polyhedra
import Graphics.Math

import Debug.Trace

----------------------------------------------------------------------

instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y)
        = [ (Vec2 x' y ) | x' <- shrink x ]
        ++ [ (Vec2 x  y') | y' <- shrink y ]

instance Arbitrary Vec3 where
    arbitrary = Vec3 <$> arbitrary <*> arbitrary <*> arbitrary
    shrink (Vec3 x y z)
        = [ (Vec3 x' y  z ) | x' <- shrink x ]
        ++ [ (Vec3 x  y' z ) | y' <- shrink y ]
        ++ [ (Vec3 x  y  z') | z' <- shrink z ]

----------------------------------------------------------------------

lenNorm1 :: Vec3 -> Property
lenNorm1 v = v /= zero ==> len (normalize v) ~= 1

projMinMaxTest :: [Vec3] -> Vec3 -> Property
projMinMaxTest vs n = not (null vs) && n /= zero ==> mn <= mx
    where (mn, mx) = projMinMax vs nn
          nn = normalize n

projFlipRel :: [Vec3] -> [Vec3] -> Vec3 -> Property
projFlipRel vs0 vs1 n = not (null vs0) && not (null vs1) && n /= zero
                      ==> rel r0 r1 == flipRel (rel r1 r0)
    where r0 = projMinMax vs0 nn
          r1 = projMinMax vs1 nn
          nn = normalize n
          flipRel AInB = BInA
          flipRel BInA = AInB
          flipRel othe = othe

randomBox :: Gen Polyhedron
randomBox = do
    let tbnd = 0
    t <- choose (vpromote (-tbnd), vpromote tbnd)
    s <- choose (vpromote 0.1, vpromote 4)
    r <- choose (0, tau)
    rv <- arbitrary `suchThat` (/= zero)
    return $ mapVertices ((+t) . rotate3 r rv . (*s)) box

collTest :: Property
collTest = forAll (vectorOf 3 randomBox) $ \[a, b, c] -> 
    relation a b == AInB && colliding (relation a c)
    ==> colliding (relation b c)

