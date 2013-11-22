{-# LANGUAGE FlexibleInstances #-}
module Graphics.SetOperations
    ( BSP, Polyhedron (..)
    , toBSP, toPolyhedron
    , margeBSP, SetOperation (..)
    , condense
    , calcPlanes
    , countBSP
) where

import Control.Arrow ((***))
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List (union)

import Debug.Trace


import Data.List (elemIndex)
import Text.Printf
import Graphics.Determinant
import Graphics.Polyhedra hiding (Relation(..))
import Graphics.Math hiding (det3)

epsilon = 1e-6

class Zeroizable a where
    zeroize :: a -> a

instance Zeroizable Scalar where
    zeroize n = if n < epsilon && n > (-epsilon) then 0 else n
instance Zeroizable Vec3 where
    zeroize = vmap zeroize

----------------------------------------------------------------------

data Plane = Plane Scalar Scalar Scalar Scalar deriving (Eq, Ord, Show)
data Face = Face Plane [Plane] deriving (Eq, Show)

----------------------------------------------------------------------

type Region = [Face]
data BSP = In | Out | Node BSP Face BSP deriving (Show)
instance Eq BSP where
    In             == In             = True
    Out            == Out            = True
    (Node la _ ra) == (Node lb _ rb) = la == lb && ra == rb
    _              == _              = False

countBSP :: BSP -> (Int, Int, Int)
countBSP In  = (1, 0, 0)
countBSP Out = (0, 1, 0)
countBSP (Node l _ r) = (lis+ris, los+ros, lns+rns+1)
    where (lis, los, lns) = countBSP l
          (ris, ros, rns) = countBSP r

createBSP :: [Face] -> BSP
createBSP [] = Out
createBSP (f:[]) = Node Out f In
createBSP (f:fs) = Node (createBSP ls) f (createBSP rs)
    where (ls, rs) = splitRegion f fs



data SetOperation
    = Union
    | Difference
    | Intersection
    -- | SymmetricDifference
    deriving (Show, Enum)

cmp :: BSP            -> BSP

cmp   In             = Out
cmp   Out            = In
cmp   (Node lt s rt) = Node (cmp lt) (s) (cmp rt)

setOperation :: SetOperation -> BSP -> BSP -> BSP

setOperation   Union          In    set = In
setOperation   Union          Out   set = set
setOperation   Union          set   In  = In
setOperation   Union          set   Out = set
                                    
setOperation   Difference     In    set = Out    
setOperation   Difference     Out   set = set      
setOperation   Difference     set   In  = cmp set 
setOperation   Difference     set   Out = Out    
                                    
setOperation   Intersection   In    set = set  
setOperation   Intersection   Out   set = Out
setOperation   Intersection   set   In  = set  
setOperation   Intersection   set   Out = Out

condense :: BSP -> BSP
condense Out = Out
condense In = In
condense (Node a f b) = if ca == cb then ca else Node ca f cb
    where
        ca = condense a
        cb = condense b

margeBSP :: SetOperation -> BSP -> BSP -> BSP
margeBSP Union a b = mBSP [] Union (cmp a) (cmp b)
margeBSP Difference a b = mBSP [] Intersection a (cmp b)
margeBSP op a b = mBSP {-bigBoxRegion-} [] op a b

splitRegion :: Face -> [Face] -> ([Face], [Face])
splitRegion s = (catMaybes *** catMaybes) . unzip . map (splitFace s)

mBSP :: [Face] -> SetOperation -> BSP -> BSP -> BSP
mBSP region op (Node lt lf rt) rn@(Node _ _ _) =
    --Node ca lf cb
    if ca == cb then ca else Node ca lf cb
    where (lp, rp) = partitionBSP (lregion, rregion) lf rn
          (lregion, rregion) = splitRegion lf region
          ca = mBSP lregion op lt lp
          cb = mBSP rregion op rt rp
mBSP _ op s1 s2 = setOperation op s1 s2

partitionBSP :: ([Face], [Face]) -> Face -> BSP -> (BSP, BSP)
partitionBSP _ _ Out = (Out, Out)
partitionBSP _ _ In  = (In, In)
partitionBSP (lregion, rregion) p (Node lt f rt)
    | coincidence p f =
        if coorientation p f then (lt, rt) else (rt, lt)
    | null lregionM = (rtM, Node lt f rtP)
    | null lregionP = (Node lt f rtM, rtP)
    | null rregionM = (ltM, Node ltP f rt)
    | null rregionP = (Node ltM f rt, ltP)
    | otherwise     = (Node ltM f rtM, Node ltP f rtP)
         --else let (lf, rf) = splitFace p f
         --    in (Node ltM (fromJust lf) rtM,
         --        Node ltP (fromJust rf) rtP)
    where
        (ltM, ltP) = partitionBSP (lregionM, lregionP) p lt
        (rtM, rtP) = partitionBSP (rregionM, rregionP) p rt

        (lregionM, lregionP) = splitRegion f lregion
        (rregionM, rregionP) = splitRegion f rregion

----------------------------------------------------------------------

orientation p q r s
    | isNaN dm1 = error $ "orientation: dm1 NaN"
    | isNaN dm2 = error $ "orientation: dm2 NaN"
    | dm1 /= 0 = dm1 * dm2
    -- | otherwise = error "Invalid point!"
    | otherwise =
        traceAs ("Warning: Invalid point!\n" ++ show [p, q, r])
        dm1 * dm2
    where
        dm1 = det3 [ptv3 p, ptv3 q, ptv3 r]
        dm2 = det4 [ptv4 p, ptv4 q, ptv4 r, ptv4 s]

ptv3 (Plane x y z _) = [x, y, z]
ptv4 (Plane x y z w) = [x, y, z, w]

data Sign = P | Z | M deriving Show
toSign n | n > 0 = P
         | n == 0 = Z
         | n < 0 = M
         | otherwise = error $ "toSign: " ++ show n

data Outp = B | HB | N deriving Show
output h b B  = [b]
output h b HB = [h,b]
output h b N  = []

clipTable _ P _ = B
clipTable P Z P = B

clipTable _ Z P = HB
clipTable _ M P = HB

clipTable _ _ _ = N

coincidence (Face (Plane sa sb sc sd) _) (Face (Plane ha hb hc hd) _)
    = all (== 0) [ sa*hb - sb*ha, sb*hc - sc*hb, sa*hc - sc*ha
                , sc*hd - sd*hc, sd*ha - sa*hd, sb*hd - sd*hb ]

coorientation (Face (Plane sa sb sc sd) _) (Face (Plane ha hb hc hd) _)
    = all (>= 0) [ sa*ha, sb*hb, sc*hc, sd*hd ] 

clipFace fh@(Face h _) fs@(Face s bs) =
    if coincidence fs fh
        then if coorientation fs fh
            then Just $ Face s bs
            else Nothing
        else if length out > 2 
            then Just $ Face s out
            else Nothing
    where
        les = length bs - 1
        pix = flip mod (les+1)
        ori q r = toSign $ orientation s q r h

        out = concat . for (zip [0..les] bs)
            $ \(i, b) -> output h b $ clipTable
                (ori (bs!!pix (i-2)) (bs!!pix (i-1)))
                (ori (bs!!pix (i-1))  b             )
                (ori  b              (bs!!pix (i+1)))

flipFacePlane (Face s bs) = Face (flipPlane s) (bs)
flipPlane (Plane a b c d) = Plane (z a) (z b) (z c) (z d)
    where z x = {-zeroize-} (-x)
splitFace h f = (clipFace h f, clipFace (flipFacePlane h) f)

----------------------------------------------------------------------

calcPlane p n@(Vec3 nx ny nz) = Plane nx ny nz (-d)
    where
        d = n `dot` p

calcPlanes :: [Vec3] -> [Facet] -> [Vec3] -> [Plane]
calcPlanes vs is ns = map (\(i, n) -> calcPlane ((vs !!) $ head i) n)
    $ zip is ns

----------------------------------------------------------------------

edges [] = []
edges es = egs es $ head es
    where egs [] h = []
          egs (e:[]) h = [[e, h]]
          egs (e:g:es) h = [e, g]:egs (g:es) h

isEdgeOf (a:b:[]) is = ieo a b is $ head is
    where ieo a b [] _ = False
          ieo a b (l:[]) h = l == a && h == b
          ieo a b (x:y:es) h = (x == a && y == b) || (ieo a b (y:es) h)

numEdges = concat
         . map (\(i, is) -> map (\e -> (i, e)) is)
         . zip [0..] . map edges

plNum  pl (i, pe) = maybeIf (pe == pl) i
plNums ixs pl = catMaybes $ map (plNum pl) (numEdges ixs)

edgePlanes ixs = map (concat . map (plNums ixs))
               . map (map reverse . edges) $ ixs

mkFaces :: [Plane] -> [[Int]] -> [Face]
mkFaces pls = map (\(s, bs) -> Face (pls!!s) (map (pls!!) bs))
            . zip [0..] . edgePlanes

toBSP :: Polyhedron -> BSP
toBSP (Polyhedron vs is ns) = createBSP fs
    where
        ps = calcPlanes vs is ns
        fs = mkFaces ps is

----------------------------------------------------------------------

maybeIf True a = Just a
maybeIf False _ = Nothing
for = flip map

----------------------------------------------------------------------

distFromPlane :: Vec3 -> Plane -> Scalar
distFromPlane v (Plane a b c d) = dotprod (Vec3 a b c) v + d

getPoint :: Plane -> Plane -> Plane -> Maybe Vec3
getPoint (Plane a1 b1 c1 d1) (Plane a2 b2 c2 d2) (Plane a3 b3 c3 d3)
    | d == 0     = Nothing
    | otherwise = Just p 
    where
        d = n1 &. cp1
        n1 = Vec3 a1 b1 c1
        n2 = Vec3 a2 b2 c2
        n3 = Vec3 a3 b3 c3
        cp1 = n2 &^ n3
        cp2 = n3 &^ n1
        cp3 = n1 &^ n2
        p = (neg $ d1*&cp1 + d2*&cp2 + d3*&cp3) &* (1/d)

type Poly = [Vec3]
faceToPoly :: Face -> Poly
faceToPoly (Face s bs) = catMaybes $ 
    for [0..lbs] $ \i -> getPoint s (bs!!(pix i)) (bs!!(pix (i+1)))
    where
        lbs = length bs - 1
        pix = flip mod (lbs+1)

faceIn :: BSP -> Face -> [Face]
faceIn Out _ = []
faceIn In  p = [p]
faceIn (Node lt f rt) p -- = faceIn lt p ++ faceIn rt p
    | coincidence f p = faceIn lt p ++ faceIn rt p
    | otherwise = (faceIn lt =<< maybeToList lp) ++
                  (faceIn rt =<< maybeToList rp)
    where (lp, rp) = splitFace f p

fromBSP :: BSP -> [Face]
fromBSP Out = []
fromBSP In = []
fromBSP (Node lt f rt) = f : (fromBSP lt ++ fromBSP rt)

inBSP :: BSP -> [Face] -> [Face]
inBSP t = concat . map (faceIn t)

convertFromBSP :: BSP -> [Face]
convertFromBSP t = inBSP t $ fromBSP t

toIndexed :: [Poly] -> ([Vec3], [[Int]])
toIndexed ps = (vs, is)
    where
        vs = Set.toList . Set.fromList $ concat ps
        is = map (catMaybes . map (flip elemIndex vs)) ps

toPolyhedron :: BSP -> Polyhedron
toPolyhedron b = Polyhedron vs is ns
    where
        (vs, is) = toIndexed $ map faceToPoly fs
        ns = for fs $ \(Face (Plane x y z d) _) -> 
                        normalize $ (Vec3 x y z)
        fs = convertFromBSP b

----------------------------------------------------------------------

dtraceItAs name r = trace (name ++ show r) r
dbg = False
traceIt r = if dbg then trace (show r) r else r
traceAs name r = if dbg then trace name r else r
traceItAs name r = if dbg then trace (name ++ show r) r else r
traceIf pred r = if pred then traceIt r else r

