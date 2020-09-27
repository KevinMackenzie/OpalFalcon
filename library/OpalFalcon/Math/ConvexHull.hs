module OpalFalcon.Math.ConvexHull
  ( convexHull2D,
    convexHull2DArea,
    convexHull3D,
    convexHull3DVolume,
    insideEdge,
    lstPairs,
    insertPoint,
    createSimplex,
    pointCloudExtrema,
    splitPoints,
    quickhull3D,
    calculateHorizon,
    QuickheapMesh (..),
    newQhMesh,
    addTri,
    quickhull3DIterate,
    quickhull3DStep,
  )
where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.ST
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Data.STRef
import qualified Data.Set as Set
import Data.Tuple
import qualified Data.Vector.Storable as VS
import qualified OpalFalcon.Math.MMesh as MM
import OpalFalcon.Math.Vector

-- TODO: re-implement the convex hull code to use vectors instead of lists for efficiency
-- TODO: Make Vec3d's storable

-- TODO: Theres an error in the convex hull code...
-- Finds the area of the convex hull containing all photons
convexHull2DArea :: [Vec3d] -> Vec3d -> Maybe Double
convexHull2DArea [] _ = Nothing -- Zero points: no area
convexHull2DArea (_ : []) _ = Nothing -- One point: no area
convexHull2DArea (_ : (_ : [])) _ = Nothing -- Two points: do something
convexHull2DArea pts norm =
  let ((o, _) : t) = convexHull2D pts norm
      -- Use the dot product to get the area projected onto the plane
      a2 = foldl (+) 0 $ map (\(p0, p1) -> norm |.| ((p0 |-| o) |><| (p1 |-| o))) t
   in Just $ a2 / 2

lstPairs' :: a -> [a] -> [(a, a)]
lstPairs' x (h : []) = [(h, x)]
lstPairs' x (h : (t@(h' : _))) = (h, h') : (lstPairs' x t)

-- Generates list of edges from an ordered list of vertices to form a closed polygon
lstPairs :: [a] -> [(a, a)]
lstPairs l@(h : _) = lstPairs' h l

insideEdge :: Vec3d -> Vec3d -> Vec3d -> Vec3d -> Bool
insideEdge p0 p1 pt norm = ((p1 |-| p0) |><| (pt |-| p0)) |.| norm >= 0

-- A set of functions that adds a point to a convex hull
removePoint :: [(Vec3d, Vec3d)] -> Vec3d -> Vec3d -> Vec3d -> [(Vec3d, Vec3d)]
removePoint [] p2 pt _ = [(pt, p2)]
removePoint ((pn, pn1) : t) _ pt norm =
  if insideEdge pn pn1 pt norm
    then (pt, pn) : ((pn, pn1) : t) -- because of convexity assumption
    else removePoint t pn1 pt norm

insertPoint' :: [(Vec3d, Vec3d)] -> Vec3d -> Vec3d -> [(Vec3d, Vec3d)]
insertPoint' [] _ _ = []
insertPoint' ((pn, pn1) : t) pt norm =
  if insideEdge pn pn1 pt norm
    then (pn, pn1) : (insertPoint' t pt norm)
    else (pn, pt) : (removePoint t pn1 pt norm)

-- The first point may be removed, so rotate the list until the first point won't be
-- TODO: this is awkward
insertPoint :: [(Vec3d, Vec3d)] -> Vec3d -> Vec3d -> [(Vec3d, Vec3d)]
insertPoint [] _ _ = []
insertPoint h@((pn, pn1) : t) pt norm =
  if insideEdge pn pn1 pt norm
    then (pn, pn1) : (insertPoint' t pt norm)
    else insertPoint (t ++ [(pn, pn1)]) pt norm

-- Finds the convex hull of a set of co-planar points
convexHull2D :: [Vec3d] -> Vec3d -> [(Vec3d, Vec3d)]
convexHull2D (p0 : (p1 : (p2 : t))) norm =
  let (p1', p2') = if insideEdge p0 p1 p2 norm then (p1, p2) else (p2, p1)
      -- The initial convex hull (triangle)
      tr = lstPairs [p0, p1', p2']
   in foldl (\el pt -> insertPoint el pt norm) tr t

-- Uses the signed-volume method
-- http://chenlab.ece.cornell.edu/Publication/Cha/icip01_Cha.pdf
meshVolume :: VS.Vector Vec3d -> [(Int, Int, Int)] -> Double
meshVolume points tris =
  sum
    $ map
      ( \((V3 x1 y1 z1), (V3 x2 y2 z2), (V3 x3 y3 z3)) ->
          (1 / 6)
            * (- (x3 * y2 * z1) + (x2 * y3 * z1) + (x3 * y1 * z2) - (x1 * y3 * z2) - (x2 * y1 * z3) + (x1 * y2 * z3))
      )
    $ map (\(a, b, c) -> (points VS.! a, points VS.! b, points VS.! c)) tris

convexHull3D :: VS.Vector Vec3d -> Maybe [(Int, Int, Int)]
convexHull3D points = (map MM.triTuple) <$> (quickhull3D points)

convexHull3DVolume :: VS.Vector Vec3d -> Maybe Double
convexHull3DVolume points = (meshVolume points) <$> (convexHull3D points)

-- TODO: Update with new MM.Tri data type
data QuickheapMesh s
  = QM
      { qmMesh :: MM.MutableMesh s,
        -- The outside sets for all polys with non-empty outside sets
        qmOutsideSets :: STRef s (Map.Map MM.Tri [Int])
      }

addTri :: QuickheapMesh s -> MM.Tri -> [Int] -> ST s Bool
addTri mesh tri outsideSet =
  do
    added <- MM.addTri (qmMesh mesh) tri
    m <- readSTRef $ qmOutsideSets mesh
    -- piggyback on the invariants of MutableMesh
    when (added && (not $ null outsideSet)) $
      do
        modifySTRef (qmOutsideSets mesh) (Map.insert tri outsideSet)
    return added

removeTriOutsideSetUnsafe :: QuickheapMesh s -> MM.Tri -> ST s [Int]
removeTriOutsideSetUnsafe mesh tri =
  do
    outsideSet <- (Map.! tri) <$> (readSTRef $ qmOutsideSets mesh)
    modifySTRef (qmOutsideSets mesh) (Map.delete tri)
    return outsideSet

removeTri :: QuickheapMesh s -> MM.Tri -> ST s [Int]
removeTri mesh tri =
  do
    removed <- MM.removeTri (qmMesh mesh) tri
    m <- readSTRef $ qmOutsideSets mesh
    -- piggyback on the invariants of MutableMesh
    case removed && (tri `Map.member` m) of
      True -> removeTriOutsideSetUnsafe mesh tri
      False -> return []

removeConnectedTris :: QuickheapMesh s -> Int -> ST s [Int]
removeConnectedTris mesh pt =
  do
    tris <- MM.removeConnectedTris (qmMesh mesh) pt
    os <- mapM (\tri -> removeTriOutsideSetUnsafe mesh tri) tris
    return $ concat os

{-# INLINE getTriPoints #-}
getTriPoints :: QuickheapMesh s -> MM.Tri -> (Vec3d, Vec3d, Vec3d)
getTriPoints (QM {qmMesh = mesh}) tri = MM.getTriPoints mesh tri

{-# INLINE idxVerts #-}
idxVerts :: QuickheapMesh s -> Int -> Vec3d
idxVerts (QM {qmMesh = mesh}) idx = MM.idxVerts mesh idx

{-# INLINE indexTriPoints #-}
indexTriPoints :: VS.Vector Vec3d -> MM.Tri -> (Vec3d, Vec3d, Vec3d)
indexTriPoints pts t = (pts VS.! a, pts VS.! b, pts VS.! c)
  where
    (a, b, c) = MM.triTuple t

newQhMesh :: VS.Vector Vec3d -> ST s (QuickheapMesh s)
newQhMesh points =
  do
    mm <- MM.new points
    outsideSets <- newSTRef Map.empty
    return $ QM {qmMesh = mm, qmOutsideSets = outsideSets}

-- Computes the precise 3D convex hull of a vector of points (returns triangle mesh)
-- This could be generalized for the 2D case as well
-- http://algolist.ru/maths/geom/convhull/qhull3d.php
quickhull3D :: VS.Vector Vec3d -> Maybe [MM.Tri]
quickhull3D points = (quickhull3D' points) <$> (createSimplex points)

-- Assumes provided seed simplex is maximal and has non-zero volume
quickhull3D' :: VS.Vector Vec3d -> (Int, Int, Int, Int) -> [MM.Tri]
quickhull3D' points (pt0, pt1, pt2, pt3) =
  let seedPolys =
        [ MM.mkTri pt0 pt2 pt1,
          MM.mkTri pt0 pt3 pt2,
          MM.mkTri pt0 pt1 pt3,
          MM.mkTri pt1 pt2 pt3
        ]
   in runST $
        do
          mesh <- newQhMesh points
          firstStep <- quickhull3DStep mesh seedPolys [0 .. (VS.length points) -1]
          case firstStep of
            Left t0 -> loopM (quickhull3DIterate mesh) t0
            Right () -> return () -- Case: The initial simplex was the convex hull
          MM.enumerateTris (qmMesh mesh)

-- With a open mesh, addes the new triangles to close the mesh and partitions
--  unclaimed points into new triangle's outside sets
quickhull3DStep :: QuickheapMesh s -> [MM.Tri] -> [Int] -> ST s (Either (MM.Tri, [Int]) ())
quickhull3DStep mesh newTris unclaimedPts =
  let (_, newOutsideSets) = foldr accumPartition (unclaimedPts, []) newTris
      accumPartition curr (inside, outsideSets) =
        (inside', outsideSet : outsideSets)
        where
          (inside', outsideSet) = splitPoints (MM.vertices $ qmMesh mesh) curr inside
   in do
        -- Add new prims from this iterations
        mapM (\(x, y) -> addTri mesh x y) $ zip newTris newOutsideSets
        outsideSets <- readSTRef $ qmOutsideSets mesh
        return $ nextTri outsideSets

-- Induction step of quickhull algorithm
quickhull3DIterate :: QuickheapMesh s -> (MM.Tri, [Int]) -> ST s (Either (MM.Tri, [Int]) ())
quickhull3DIterate mesh@(QM {qmMesh = m}) (t, oPts) =
  let comparePointToPlane tri x = pointToPlaneSigned (getTriPoints mesh tri) ((MM.vertices m) VS.! x)
      eyePt = maximumBy (comparing $ comparePointToPlane t) oPts
   in do
        (hEdges, unclaimedPts) <- calculateHorizon mesh eyePt t
        let newTris = map (\(pt1, pt2) -> MM.mkTri eyePt pt1 pt2) hEdges
         in quickhull3DStep mesh newTris unclaimedPts

calculateHorizon :: QuickheapMesh s -> Int -> MM.Tri -> ST s ([(Int, Int)], [Int])
calculateHorizon mesh eyePoint currFace =
  do
    removedEdges <- newSTRef $ Set.empty
    horizonEdges <- newSTRef $ Set.empty
    unclaimedPts <- newSTRef $ Set.empty
    calculateHorizon' removedEdges horizonEdges unclaimedPts mesh eyePoint Nothing currFace
    modifySTRef unclaimedPts (Set.delete eyePoint)
    unclaimedPts' <- readSTRef unclaimedPts
    horizonEdges' <- readSTRef horizonEdges
    -- Note: swap the horizon edges because when they are added, they are on a back-facing triangle
    return (map swap $ Set.toList horizonEdges', Set.toList unclaimedPts')

calculateHorizon' :: STRef s (Set.Set (Int, Int)) -> STRef s (Set.Set (Int, Int)) -> STRef s (Set.Set Int) -> QuickheapMesh s -> Int -> Maybe (Int, Int) -> MM.Tri -> ST s ()
calculateHorizon' removedEdges horizonEdges unclaimedPts mesh eyePoint crossedEdge currFace =
  let isVisible = pointToPlaneSigned (getTriPoints mesh currFace) (idxVerts mesh eyePoint) > 0
      recurse (e1, e2) =
        do
          nextPt <- MM.crossEdge (qmMesh mesh) e1 e2
          case nextPt of
            Nothing -> return () -- Hit a boundary edge
                -- Be careful to flip the edge direction
            Just pt -> calculateHorizon' removedEdges horizonEdges unclaimedPts mesh eyePoint (Just (e2, e1)) (MM.mkTri e2 e1 pt)
   in do
        onHull <- MM.hasTri (qmMesh mesh) currFace
        if not onHull -- The crossed edge will only be Nothing on the entry call
          then modifySTRef removedEdges (Set.insert $ fromJust crossedEdge)
          else
            if not isVisible -- We crossed a horizon edge if it isn't visible
              then modifySTRef horizonEdges (Set.insert $ fromJust crossedEdge)
              else do
                outsideSet <- removeTri mesh currFace
                modifySTRef unclaimedPts (Set.union (Set.fromList outsideSet))
                others <- case crossedEdge of
                  Nothing -> return $ MM.triEdges currFace
                  Just e@(e0, _) -> do
                    modifySTRef removedEdges (Set.insert e)
                    return $ MM.otherTriEdges currFace e0
                _ <- mapM recurse others
                return ()

nextTri :: Map.Map MM.Tri [Int] -> Either (MM.Tri, [Int]) ()
nextTri s =
  if Map.null s
    then Right ()
    else Left $ Map.findMax s

-- Splits points into the (inside, outside) set
splitPoints :: VS.Vector Vec3d -> MM.Tri -> [Int] -> ([Int], [Int])
splitPoints points tri idxs =
  partition (\x -> (pointToPlaneSigned (indexTriPoints points tri) (points VS.! x)) <= 0) idxs

-- Find all extrema of a point cloud all in one go
pointCloudExtrema :: [(Vec3d -> Vec3d -> Bool)] -> VS.Vector Vec3d -> [(Int, Int)]
pointCloudExtrema lte points =
  let numExtrema = length lte
      ext mns x fs =
        map
          (\(mn, a) -> if (points VS.! x) `a` (points VS.! mn) then x else mn)
          $ zip mns fs
      min' mns x = ext mns x lte
      max' mxs x = ext mxs x $ map (\f -> (\a b -> not $ f a b)) lte
      len = VS.length points
      f i mn mx =
        if i == len
          then zip mn mx
          else f (i + 1) (min' mn i) (max' mx i)
   in f 1 (replicate numExtrema 0) (replicate numExtrema 0)

-- The first three points represent a CCW-normal triangle and the 4th point is 'above'
createSimplex :: VS.Vector Vec3d -> Maybe (Int, Int, Int, Int)
createSimplex points =
  let vecCompComp f x y = (f x) <= (f y)
      extrema = pointCloudExtrema (map vecCompComp [xPos, yPos, zPos]) points
      extremaList = dropWhile (\(mn, mx) -> (points VS.! mn) == (points VS.! mx)) extrema
      vec = normalize $ v1 |-| v0
      (pt0, pt1) = head extremaList
      pt2 = VS.maxIndexBy (comparing (pointToLine v0 vec)) points
      pt3 = VS.maxIndexBy (comparing (pointToPlane (v0, v1, v2))) points
      v0 = points VS.! pt0
      v1 = points VS.! pt1
      v2 = points VS.! pt2
      v3 = points VS.! pt3
      pt3Dist = pointToPlaneSigned (v0, v1, v2) v3
   in if null extremaList
        then Nothing -- degenerate case (0d)
        else
          if pointToLine v0 vec v2 == 0
            then Nothing -- degenerate case (1d)
            else
              if pt3Dist == 0
                then Nothing -- degenerate case (2d)
                else
                  Just $
                    if pt3Dist < 0
                      then (pt2, pt1, pt0, pt3) -- Negative sign, flip triangle
                      else (pt0, pt1, pt2, pt3)
