module OpalFalcon.Math.ConvexHull
  ( convexHull2D,
    convexHull2DArea,
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

data Tri = Tri Int Int Int deriving (Eq)

asTuple :: Tri -> (Int, Int, Int)
asTuple (Tri a b c) = (a, b, c)

otherEdges :: Tri -> (Int, Int) -> [(Int, Int)]
otherEdges (Tri a b c) (p0, p1) =
  let e0 = (a, b)
      e1 = (b, c)
      e2 = (c, a)
   in if p0 == a then [e1, e2] else if p0 == b then [e2, e0] else [e0, e1]

instance Ord Tri where
  (<) (Tri a b c) (Tri d e f) = (a + b + c) < (d + e + f)

data QuickheapMesh s
  = QM
      { qmMesh :: MM.MutableMesh s,
        -- The outside sets for all polys with non-empty outside sets
        qmOutsideSets :: STRef s (Map.Map Tri [Int])
      }

addTri :: QuickheapMesh s -> Tri -> [Int] -> ST s Bool
addTri mesh tri@(Tri p0 p1 p2) outsideSet =
  do
    added <- MM.addTri (qmMesh mesh) p0 p1 p2
    m <- readSTRef $ qmOutsideSets mesh
    -- piggyback on the invariants of MutableMesh
    when (added && (not $ Map.member tri m)) $
      do
        modifySTRef (qmOutsideSets mesh) (Map.insert tri outsideSet)
    return added

removeTriOutsideSetUnsafe :: QuickheapMesh s -> Tri -> ST s [Int]
removeTriOutsideSetUnsafe mesh tri =
  do
    outsideSet <- (Map.! tri) <$> (readSTRef $ qmOutsideSets mesh)
    modifySTRef (qmOutsideSets mesh) (Map.delete tri)
    return outsideSet

removeTri :: QuickheapMesh s -> Tri -> ST s [Int]
removeTri mesh tri@(Tri p0 p1 p2) =
  do
    removed <- MM.removeTri (qmMesh mesh) p0 p1 p2
    m <- readSTRef $ qmOutsideSets mesh
    -- piggyback on the invariants of MutableMesh
    case removed && (tri `Map.member` m) of
      True -> removeTriOutsideSetUnsafe mesh tri
      False -> return []

removeConnectedTris :: QuickheapMesh s -> Int -> ST s [Int]
removeConnectedTris mesh pt =
  do
    ps <- MM.removeConnectedTris (qmMesh mesh) pt
    os <- mapM (\(p1, p2) -> removeTriOutsideSetUnsafe mesh $ Tri pt p1 p2) ps
    return $ concat os

getTriPoints :: QuickheapMesh s -> Tri -> (Vec3d, Vec3d, Vec3d)
getTriPoints (QM {qmMesh = mesh}) (Tri p0 p1 p2) = MM.getTriPoints mesh p0 p1 p2

newQhMesh :: VS.Vector Vec3d -> ST s (QuickheapMesh s)
newQhMesh points =
  do
    mm <- MM.new points
    outsideSets <- newSTRef Map.empty
    return $ QM {qmMesh = mm, qmOutsideSets = outsideSets}

-- Computes the precise convex hull of a vector of points (returns triangle mesh)
-- http://algolist.ru/maths/geom/convhull/qhull3d.php
quickhull3D :: VS.Vector Vec3d -> [(Int, Int, Int)]
quickhull3D points =
  let (pt0, pt1, pt2, pt3) = createSimplex points
      seedPolys = [Tri pt0 pt2 pt1, Tri pt0 pt3 pt2, Tri pt0 pt1 pt3, Tri pt1 pt2 pt3]
      accumPartition (inside, outsideSets) curr =
        (inside', outsideSet : outsideSets)
        where
          (inside', outsideSet) = splitPoints points curr inside
      (_, outsideSets) = foldl accumPartition ([1 .. (VS.length points)], []) seedPolys
      comparePointToPlane (Tri pt0 pt1 pt2) x = pointToPlaneSigned (points VS.! pt0, points VS.! pt1, points VS.! pt2) (points VS.! x)
      iterate mesh (t, oPts) =
        let eyePt = maximumBy (comparing $ comparePointToPlane t) oPts
         in do
              (hEdges, unclaimedPts) <- calculateHorizon mesh eyePt t
              -- Remove all prims with points marked for removal from the poly
              -- unclaimedPts <- concat <$> (mapM (removeConnectedTris mesh) unhulledPts)
              let newTris = map (\(pt1, pt2) -> Tri eyePt pt1 pt2) hEdges
                  (_, newOutsideSets) = foldl accumPartition (unclaimedPts, []) newTris
               in do
                    -- Add new prims from this iterations
                    mapM (\(x, y) -> addTri mesh x y) $ zip newTris newOutsideSets
                    outsideSets <- readSTRef $ qmOutsideSets mesh
                    return $ nextTri outsideSets
   in runST $
        do
          mesh <- newQhMesh points
          mapM (\(x, y) -> addTri mesh x y) $ zip seedPolys outsideSets
          initialOutsideSets <- readSTRef $ qmOutsideSets mesh
          case nextTri initialOutsideSets of
            Left t0 -> loopM (iterate mesh) t0
            Right () -> return () -- Case: The initial simplex was the convex hull
          MM.enumerateTris (qmMesh mesh)

calculateHorizon :: QuickheapMesh s -> Int -> Tri -> ST s ([(Int, Int)], [Int])
calculateHorizon mesh eyePoint currFace =
  do
    removedEdges <- newSTRef $ Set.empty
    horizonEdges <- newSTRef $ Set.empty
    unclaimedPts <- newSTRef $ Set.empty
    calculateHorizon' removedEdges horizonEdges unclaimedPts mesh eyePoint Nothing currFace
    unclaimedPts' <- readSTRef unclaimedPts
    horizonEdges' <- readSTRef horizonEdges
    return (Set.toList horizonEdges', Set.toList unclaimedPts')

calculateHorizon' :: STRef s (Set.Set (Int, Int)) -> STRef s (Set.Set (Int, Int)) -> STRef s (Set.Set Int) -> QuickheapMesh s -> Int -> Maybe (Int, Int) -> Tri -> ST s ()
calculateHorizon' removedEdges horizonEdges unclaimedPts mesh eyePoint crossedEdge currFace@(Tri pt0 pt1 pt2) =
  let points = MM.vertices $ qmMesh mesh
      isVisible = pointToPlaneSigned (getTriPoints mesh currFace) (points VS.! eyePoint) > 0
      recurse (e1, e2) =
        do
          nextPt <- MM.crossEdge (qmMesh mesh) e1 e2
          case nextPt of
            Nothing -> return () -- Hit a boundary edge
                -- Be careful to flip the edge direction
            Just pt -> calculateHorizon' removedEdges horizonEdges unclaimedPts mesh eyePoint (Just (e2, e1)) (Tri e2 e1 pt)
   in do
        onHull <- MM.hasTri (qmMesh mesh) pt0 pt1 pt2
        if not onHull -- The crossed edge will only be Nothing on the entry call
          then modifySTRef removedEdges (Set.insert $ fromJust crossedEdge)
          else
            if not isVisible -- We crossed a horizon edge if it isn't visible
              then modifySTRef horizonEdges (Set.insert $ fromJust crossedEdge)
              else do
                outsideSet <- removeTri mesh currFace
                modifySTRef unclaimedPts (Set.union (Set.fromList outsideSet))
                others <- case crossedEdge of
                  Nothing -> return [(pt0, pt1), (pt1, pt2), (pt2, pt0)]
                  Just e -> do
                    modifySTRef removedEdges (Set.insert e)
                    return $ otherEdges currFace e
                _ <- mapM recurse others
                return ()

nextTri :: Map.Map Tri [Int] -> Either (Tri, [Int]) ()
nextTri s =
  if Map.null s
    then Right ()
    else Left $ Map.findMax s

-- Splits points into the (inside, outside) set
splitPoints :: VS.Vector Vec3d -> Tri -> [Int] -> ([Int], [Int])
splitPoints points (Tri p0 p1 p2) idxs =
  let v0 = points VS.! p0
      v1 = points VS.! p1
      v2 = points VS.! p2
   in partition (\x -> (pointToPlaneSigned (v0, v1, v2) (points VS.! x)) <= 0) idxs

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

-- The first three points represent a CCW-inward triangle and the 4th point is 'inside'
createSimplex :: VS.Vector Vec3d -> (Int, Int, Int, Int)
createSimplex points =
  let vecCompComp f x y = (f x) <= (f y)
      extrema = pointCloudExtrema (map vecCompComp [xPos, yPos, zPos]) points
      -- NOTE: This will error in the degenerate case (0d)
      (pt0, pt1) = head $ dropWhile (\(mn, mx) -> (points VS.! mn) == (points VS.! mx)) extrema
      vec = normalize $ v1 |-| v0
      pt2 = VS.maxIndexBy (comparing (pointToLine v0 vec)) points
      pt3 = VS.maxIndexBy (comparing (pointToPlane (v0, v1, v2))) points
      v0 = points VS.! pt0
      v1 = points VS.! pt1
      v2 = points VS.! pt2
      v3 = points VS.! pt3
      pt3Dist = pointToPlaneSigned (v0, v1, v2) v3
   in if pointToLine v0 vec v2 == 0
        then undefined -- degenerate case (1d)
        else
          if pt3Dist == 0
            then undefined -- degenerate case (2d)
            else
              if pt3Dist < 0
                then (pt2, pt1, pt0, pt3) -- Negative sign, flip triangle
                else (pt0, pt1, pt2, pt3)
