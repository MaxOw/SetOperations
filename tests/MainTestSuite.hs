module Main (main) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Graphics.PolyhedraTests
import Graphics.SetOperationsTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Polyhedra"
        [ testProperty "Normalized vector length equals 1" lenNorm1
        , testProperty "projMinMax: min <= max" projMinMaxTest
        , testProperty "rel a b == flipRel (rel b a)" projFlipRel
        , testProperty "a in b and a col c -> b col c" collTest ]
    --, testGroup "SetOperations"
        --[ testProperty "toPolyhedron . toBSP ~= id" convIdentity ]
    ]

