module Graphics.SetOperationsTests where

import Test.QuickCheck

import Graphics.Math
import Graphics.Polyhedra
import Graphics.SetOperations

import Graphics.PolyhedraTests

import Data.List (sort)
import Data.Function (on)
import Debug.Trace
import Text.Printf

----------------------------------------------------------------------

pv3 :: Vec3 -> String
pv3 (Vec3 x y z) = printf "x:%+.4f y:%+.4f z:%+.4f" x y z

pp :: Vec3 -> Vec3 -> String
pp p0 p1 = pv3 p0 ++ "  |  " ++ pv3 p1 ++ "\n"

printPair :: Polyhedron -> Polyhedron -> String
printPair p0 p1
    = "Vertices:\n"
    ++ (foldl1 (++) $ on (zipWith pp) (sort . oVertices) p0 p1) ++ "\n"
    ++ "Normals:\n"
    ++ (foldl1 (++) $ on (zipWith pp) (sort . oNormals) p0 p1) ++ "\n"

convIdentity :: Property
convIdentity = forAll randomBox tsp
    -- $ \a -> toPolyhedron (toBSP a) ~= a
    where
        tsp a = trace (printPair a (conv a)) $ conv a ~= a
        --tsp a = trace (show (ps a)) $ conv a ~= a
        ps (Polyhedron v i n) = calcPlanes v i n
        conv = toPolyhedron . toBSP
