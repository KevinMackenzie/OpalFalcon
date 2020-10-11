module OpalFalcon.Geometry.Extrusions where

import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import qualified OpalFalcon.Math.MMesh as MM
import qualified OpalFalcon.Math.TriMesh as TMesh
import qualified OpalFalcon.Scene.Objects.Triangle as T
import OpalFalcon.Scene.Objects
import OpalFalcon.Math.Vector

-- extrudes a curve in a direction by the length of that vector
-- The normal will be "up" if 'dir" is forward and pts go right
extrudeIndexedPolyLine :: VS.Vector Vec3d -> VS.Vector Int -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
extrudeIndexedPolyLine pts idxs dir = extrudePolyLine (VS.map (pts VS.!) idxs) dir

extrudePolyLine :: VS.Vector Vec3d -> Vec3d -> (VS.Vector Vec3d, VB.Vector MM.Tri)
extrudePolyLine points dir =
  let l = VS.length points
      oppPoints = VS.map (dir |+|) points
      tris =
        concat $
          map
            ( \idx ->
                [ MM.mkTri idx (idx + 1) (l + idx),
                  MM.mkTri (idx + 1) (l + idx + 1) (l + idx)
                ]
            )
            [0 .. VS.length points -2]
   in (points VS.++ oppPoints, VB.fromList tris)

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
