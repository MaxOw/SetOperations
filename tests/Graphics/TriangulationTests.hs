module Graphics.TriangulationTests where

import Test.QuickCheck
import Text.Printf
import Data.List

import Graphics.Math
import Graphics.Polyhedra
import Graphics.Triangulation
import Graphics.Triangulation.KET

import Graphics.PolyhedraTests

disimilar [] = True
disimilar (a:as) = all (/= a) as && disimilar as

nonUnilinear ps
    = (/= genericLength ps) . sum $ map (len . normalize) ps

triNormal ps = let [a, b, c] = take 3 ps in
    normalize $ (a - b) `cross` (c - b)

class Show a => PPrint a where
    pp :: a -> String

instance PPrint Vec2 where
    pp (Vec2 x y) = printf "x:%+.6f y:%+.6f" x y
instance PPrint Vec3 where
    pp (Vec3 x y z) = printf "x:%+.6f y:%+.6f z:%+.6f" x y z
instance PPrint a => PPrint [a] where
    pp = unlines . map pp

convTest :: (Vec3, Vec3, Vec3) -> Property
convTest (a, b, c) =
    whenFail (putStrLn $ pp tps) $
    nonUnilinear ps && disimilar ps
    ==> tps ~= ps
    where
        tps = convId ps $ triNormal ps
        ps = [a, b, c]

trigSq :: Property
trigSq = forAll (vectorOf 4 arbitrary) trs
    where
        trs ps =
            whenFail (putStrLn $ pp tps ++ "\n") $
            nonUnilinear ps && disimilar ps ==>
            length tps <= 2
            where
                tps = trig2 ps

trigBox :: Property
trigBox = forAll (randomBox) trb
    where
        trb b =
            whenFail (putStrLn $ show fs ++ "\n") $
            fs == 12
            where
                tb = triangulate b
                fs = length $ oFaces tb

