module Graphics.Polyhedra where

import Data.Function (on)
import qualified Data.Set as Set
import Graphics.Math

----------------------------------------------------------------------

type Index    = Int
type Vertices = [Vec3] -- Array Index Vec3
type Facet    = [Index]
data Polyhedron = Polyhedron
    { oVertices    :: Vertices
    , oFaces       :: [Facet]
    , oNormals     :: [Vec3] }
    deriving Show

instance AlmostEq Polyhedron where
    a ~= b = on (~=) oVertices a b && on (~=) oNormals a b

mapVertices :: (Vec3 -> Vec3) -> Polyhedron -> Polyhedron
mapVertices f (Polyhedron vs is ns) = Polyhedron nvs is nns
    where
        nvs = map f vs
        nns = map (cn . take 3 . map (nvs !!)) is
        cn [p0, p1, p2] = normalize $ (p0 - p1) `cross` (p0 - p2)

----------------------------------------------------------------------

proj :: Vec3 -> Vec3 -> Vec3
proj a b = (a `dot` b) *& b

projMinMax :: [Vec3] -> Vec3 -> (Scalar, Scalar)
projMinMax vs p = (minimum ss, maximum ss)
    where
        ps = map (flip proj p) vs
        ns = map normalize ps
        ls = map len ps
        ss = map (\(n,l) -> if n ~= p then l else -l) $ zip ns ls

data Relation = On | AInB | BInA | Out deriving (Eq, Show)
rel :: (Scalar, Scalar) -> (Scalar, Scalar) -> Relation
rel (m0, x0) (m1, x1) =
    if x0 <= m1 || m0 >= x1
        then Out 
        else if (m0 < m1 && x0 <= x1) || (m0 >= m1 && x0 > x1)
            then On
            else if m0 >= m1 && x0 <= x1
                then AInB
                else BInA

fullRel :: [Relation] -> Relation
fullRel rs
    | any (== Out) rs  = Out
    | all (== AInB) rs = AInB
    | all (== BInA) rs = BInA
    | otherwise       = On

colliding :: Relation -> Bool
colliding Out = False
colliding _   = True

relation :: Polyhedron -> Polyhedron -> Relation
relation a b = fullRel $ map nrel uniqueNormals
    where
        unique = Set.toList . Set.fromList
        uniqueNormals = unique $ (oNormals a) ++ (oNormals b)
        nrel n = rel (projMinMax (oVertices a) n)
                     (projMinMax (oVertices b) n)

----------------------------------------------------------------------

recalcNormals :: Polyhedron -> Polyhedron
recalcNormals (Polyhedron vs is _) = Polyhedron vs is ns
    where
        ns = map (cn . take 3 . map (vs !!)) is
        cn [p0, p1, p2] = normalize $ (p0 - p1) `cross` (p0 - p2)

box = recalcNormals $ Polyhedron vs is []
    where
        vs = map (vmap (subtract 0.5)) $
             [ Vec3 0 0 0 , Vec3 0 1 0 , Vec3 0 1 1 , Vec3 0 0 1
             , Vec3 1 0 0 , Vec3 1 1 0 , Vec3 1 1 1 , Vec3 1 0 1 ]

        is = [ [ 0, 3, 2, 1 ]
             , [ 0, 4, 7, 3 ]
             , [ 0, 1, 5, 4 ]
             , [ 4, 5, 6, 7 ]
             , [ 5, 1, 2, 6 ]
             , [ 3, 7, 6, 2 ] ]

cylinder nn = recalcNormals $ Polyhedron vs is []
    where
        vs = [ct] ++ mrot ct ++ [cb] ++ mrot cb
        cb = Vec3 0 0 (-0.5)
        ct = Vec3 0 0   0.5 
        n = fromIntegral nn
        mrot v = map (uncurry rot) . zip (repeat v)
                                   $ map ((*tau) . (/n)) [0..n-1]
        rot (Vec3 _ _ z) i = Vec3 (r*sin(i)) (r*cos(i)) z
        r = 0.5

        is = (map reverse $ lid 0 topc) ++ lid (nn+1) botc ++ sides
        topc = [1..nn]
        --botc = [2*nn+1..nn+2]
        botc = [nn+2..2*nn+1]
        pairs ns = zip ns $ drop 1 ns ++ [head ns]
        lid st ns = map (\(a, b) -> [st, a, b]) $ pairs ns
        sides = map (\((a, b), (c, d)) -> [a, b, d, c])
              $ on zip pairs topc botc

cyli nn = recalcNormals $ Polyhedron vs is []
    where
        vs = mrot ct ++ mrot cb
        cb = Vec3 0 0 (-0.5)
        ct = Vec3 0 0   0.5 
        n = fromIntegral nn
        mrot v = map (uncurry rot) . zip (repeat v)
                                   $ map ((*tau) . (/n)) [0..n-1]
        rot (Vec3 _ _ z) i = Vec3 (r*sin(i)) (r*cos(i)) z
        r = 0.5

        is = [reverse topc] ++ [botc] ++ sides
        topc = [0..nn-1]
        --botc = [2*nn+1..nn+2]
        botc = [nn..2*nn-1]
        pairs ns = zip ns $ drop 1 ns ++ [head ns]
        sides = map (\((a, b), (c, d)) -> [a, b, d, c])
              $ on zip pairs topc botc

