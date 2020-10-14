module OpalFalcon.Geometry.Extrusions where

import Control.Monad.Extra (whileM)
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified OpalFalcon.Geometry.Curves as C
import qualified OpalFalcon.Geometry.Triangle as T
import qualified OpalFalcon.Math.MMesh as MM
import qualified OpalFalcon.Math.TriMesh as TMesh
import OpalFalcon.Math.Vector
import OpalFalcon.Scene.Objects
import qualified OpalFalcon.Scene.Objects.Triangle as T
import qualified OpalFalcon.Util.MutableList as MList

-- NOTE: does not enforce manifold condition on extruded edges
-- extrudes a curve in a direction by the length of that vector
-- The normal will be "up" if 'dir" is forward and pts go right
extrudeIndexedPolyLine :: VS.Vector Vec3d -> VS.Vector Int -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
extrudeIndexedPolyLine pts idxs dir =
  let l = VS.length pts
      oppPoints = VS.map (dir |+|) pts
      tris =
        concat $
          map
            ( \idx ->
                let idx0 = idxs VS.! idx
                    idx1 = idxs VS.! (idx + 1)
                    idx2 = l + idx1
                    idx3 = l + idx0
                 in [ MM.mkTri idx1 idx0 idx2,
                      MM.mkTri idx2 idx0 idx3
                    ]
            )
            [0 .. VS.length idxs -2]
   in (pts VS.++ oppPoints, VB.fromList tris)

-- TODO: to keep the shape closed, need to "share" indexed points
extrudePolyLine :: VS.Vector Vec3d -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
extrudePolyLine points dir = extrudeIndexedPolyLine points (VS.fromList [0 .. VS.length points -1]) dir

-- NOTE: assumes the points are coplanar; probably shouldn't
-- NOTE: Assumes the polygon is convex; probably shouldn't
-- Extrudes a closed polygon into a solid;  Connects last index to first index
extrudeIndexedPoly :: VS.Vector Vec3d -> VS.Vector Int -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
extrudeIndexedPoly points idxs dir =
  let (points', tris) = extrudeIndexedPolyLine points (VS.snoc idxs $ VS.head idxs) dir
      frontFaceTris = tesselateIndexedPoly points' idxs (negateVec dir)
      backFaceIdxs = VS.reverse $ VS.map (VS.length points +) idxs
      backFaceTris = tesselateIndexedPoly points' backFaceIdxs dir
   in (points', tris VB.++ frontFaceTris VB.++ backFaceTris)

extrudePoly :: VS.Vector Vec3d -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
extrudePoly points dir =
  extrudeIndexedPoly points (VS.fromList $ [0 .. VS.length points -1]) dir

-- NOTE: Assumes the indexs are provided in CCW order
-- NOTE: Assumes a single polygon with no holes
-- NOTE: No two adjacent points can be coincident
-- TODO: This could be extended to higher dimmensions
-- This is a little algorithm I designed: Runtime: O(n^2)? idk
--      Initial condition: A closed polygon with points ordered CCW
--      Invariant: An "inner" polygon with CCW ordering is maintained
--      Each iteration: exclude a triangular section from the inner polygon
tesselateIndexedPoly :: VS.Vector Vec3d -> VS.Vector Int -> Vec3d -> VB.Vector MM.Tri
tesselateIndexedPoly points idxs dir =
  let excludeTri mesh lRef =
        do
          l0 <- readSTRef lRef
          l1 <- MList.nextRef l0
          l2 <- MList.nextRef l1
          idx0 <- MList.readRef l0
          idx1 <- MList.readRef l1
          idx2 <- MList.readRef l2
          let pt0 = points VS.! idx0
              pt1 = points VS.! idx1
              pt2 = points VS.! idx2
              hasPoint pt = T.containsPoint (T.Tri pt0 pt1 pt2) $ points VS.! pt
              tryNext = do
                writeSTRef lRef l1
                excludeTri mesh lRef
           in if (((pt1 |-| pt0) |><| (pt2 |-| pt0)) |.| dir) > 0 -- fast check
                then do
                  n <- MList.nextRef l2
                  anyInside <- or <$> (MList.mapSlice hasPoint n l0)
                  if not anyInside -- slow check
                    then do
                      MM.addTri mesh (MM.mkTri idx0 idx1 idx2)
                      _ <- MList.erase l1
                      shouldContinue lRef
                    else tryNext
                else tryNext
      shouldContinue lRef =
        do
          l <- readSTRef lRef -- The 'inner' polygon
          len <- MList.length l -- The mesh of triangles outside polygon
          return $ len > 3
   in runST $ do
        lRef <- (MList.fromList $ VS.toList idxs) >>= newSTRef
        mesh <- MM.new points
        whileM (excludeTri mesh lRef)
        [p0, p1, p2] <- readSTRef lRef >>= MList.toList
        MM.addTri mesh (MM.mkTri p0 p1 p2)
        VB.fromList <$> MM.enumerateTris mesh

-- F must be positive and monotone
-- TODO: This could be improved by lining the bottom with points at the midpoint
--  of each interval which drops the monotone requirement
genPathBoundary :: Vec3d -> Vec3d -> Vec3d -> (Double -> Double) -> Int -> TMesh.TriMesh
genPathBoundary origin forward right f steps =
  let step = (mag right) / (fromIntegral steps)
      wDir = normalize right
      uDir = normalize $ right |><| forward
      fPoints =
        map
          ( \idx ->
              let x = (fromIntegral idx) * step
               in origin |+| (x *| wDir) |+| ((f x) *| uDir)
          )
          [0 .. steps]
      bottomLeft = origin
      bottomRight = origin |+| right
      line = VS.fromList $ (bottomLeft : fPoints) ++ [bottomRight]
      lineLen = VS.length line
      (allPoints, tris) = extrudePolyLine line forward
      endCaps0 =
        VB.fromList $
          map
            ( \idx ->
                MM.mkTri 0 (idx + 2) (idx + 1)
            )
            [0 .. steps]
      endCaps1 =
        VB.fromList $
          map
            ( \idx ->
                MM.mkTri lineLen (idx + 1 + lineLen) (idx + 2 + lineLen)
            )
            [0 .. steps]
   in TMesh.new allPoints (tris VB.++ endCaps0 VB.++ endCaps1)

mkQuadPrism pos xDir yDir zDir (V3 sx sy sz) mat =
  let pts@[lbb, lbf, ltb, ltf, rbb, rbf, rtb, rtf] =
        [ (pos |+| (xDir |* x) |+| (yDir |* y) |+| (zDir |* z))
          | x <- [- sx, sx],
            y <- [- sy, sy],
            z <- [- sz, sz]
        ]
      triples = [(lbb, lbf, ltb), (lbf, ltf, ltb), (ltf, lbf, rbf), (ltf, rbf, rtf), (rtf, rbf, rbb), (rtf, rbb, rtb), (rtb, rbb, lbb), (rtb, lbb, ltb), (ltf, rtf, rtb), (ltf, rtb, ltb), (lbf, rbb, rbf), (lbf, lbb, rbb)]
   in map (\(a, b, c) -> mkTriangleObject (T.MkTriangle a b c) mat) triples
