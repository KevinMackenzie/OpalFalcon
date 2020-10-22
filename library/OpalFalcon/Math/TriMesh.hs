module OpalFalcon.Math.TriMesh
  ( TriMesh (vertices, edges, polys, tris),
    new,
    fromMutableMesh,
    TriMeshMat,
    hittestTriMeshInside,
    hittestTriMeshOutside,
    triNorm,
    exit,
    baryToWorld,
    merge,
  )
where

import Control.Monad.ST
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VS
import Debug.Trace
import OpalFalcon.BaseTypes
import qualified OpalFalcon.Geometry.Triangle as Tri
import qualified OpalFalcon.Math.MMesh as MM
import OpalFalcon.Math.Optics
import OpalFalcon.Math.Vector
import qualified OpalFalcon.Scene.Objects.Triangle as T
import OpalFalcon.Util.Misc

-- A closed triangle mesh object
-- TODO: this assumes, not enforces, the closed mesh property
data TriMesh
  = TM
      { vertices :: VS.Vector Vec3d,
        edges :: VB.Vector (VS.Vector Int),
        polys :: VB.Vector (VB.Vector (Int, Int)),
        tris :: VB.Vector MM.Tri
      }
  deriving (Show)

fromMutableMesh :: MM.MutableMesh s -> ST s TriMesh
fromMutableMesh mm =
  do
    ts <- MM.enumerateTris mm
    es <- MM.freezeEdges mm
    ps <- MM.freezePolys mm
    return $ TM
      { vertices = MM.vertices mm,
        edges = es,
        polys = ps,
        tris = VB.fromList ts
      }

-- Merges multiple meshes into one (useful for when manually constructing meshes
--    out of logical submeshes)
merge :: [TriMesh] -> TriMesh
merge meshes =
  let offsets =
        reverse $ fst
          $ foldl
            ( \(lst, accum) new ->
                (accum : lst, VS.length new + accum)
            )
            ([], 0)
          $ map vertices meshes
      offsetTri o tri =
        let (a, b, c) = MM.triTuple tri
         in MM.mkTri (a + o) (b + o) (c + o)
      allVerts = VS.concat $ map vertices meshes
      allTris =
        VB.concat
          $ map
            ( \(offset, tris) ->
                VB.map (offsetTri offset) tris
            )
          $ zip offsets
          $ map tris meshes
   in new allVerts allTris

new :: VS.Vector Vec3d -> VB.Vector MM.Tri -> TriMesh
new points ts =
  runST $ do
    mm <- MM.new points
    mapM_ (MM.addTri mm) ts
    fromMutableMesh mm

{-# INLINE triNorm #-}
triNorm :: TriMesh -> Int -> UVec3d
triNorm mesh idx =
  let (a, b, c) = index mesh idx
   in norm3 $ (b |-| a) |><| (c |-| a)

{-# INLINE index #-}
index :: TriMesh -> Int -> (Vec3d, Vec3d, Vec3d)
index mesh idx =
  let (a, b, c) = MM.triTuple $ (tris mesh) VB.! idx
   in ((vertices mesh) VS.! a, (vertices mesh) VS.! b, (vertices mesh) VS.! c)

{-# INLINE indexAsScTri #-}
indexAsScTri :: TriMesh -> Int -> T.Triangle
indexAsScTri mesh idx =
  let (a, b, c) = index mesh idx
   in T.MkTriangle a b c

{-# INLINE baryToWorld #-}
baryToWorld :: TriMesh -> Int -> Vec3d -> Vec3d
baryToWorld mesh idx bc =
  let (a, b, c) = index mesh idx
   in Tri.baryToWorld (Tri.Tri a b c) bc

type TriMeshMat = TriMesh -> Int -> Vec3d -> AppliedMaterial

triMeshTriMat :: TriMeshMat -> TriMesh -> Int -> T.Triangle -> Vec3d -> AppliedMaterial
triMeshTriMat tmm tm idx _ bc = tmm tm idx bc

hittestTriMeshOutside :: TriMesh -> Ray -> Maybe Hit
hittestTriMeshOutside = hittestTriMesh T.hittestTriangleFront

hittestTriMeshInside :: TriMesh -> Ray -> Maybe Hit
hittestTriMeshInside tm r = (flipHitNorm) <$> (hittestTriMesh T.hittestTriangleBack tm r)

-- TODO: this is being asked to "exit" from points that are not inside the mesh...
-- The triangle intersection returns barycentric coords on the triangle, and not world-space position
intersectTriMeshInside :: TriMesh -> Ray -> Maybe Double
intersectTriMeshInside mesh r =
  let intersections =
        map
          ( \tri ->
              let (a, b, c) = index mesh tri
               in T.intersectTriangle (T.MkTriangle a c b) r
          )
          [0 .. VB.length (tris mesh) -1]
   in case filterJust intersections of
        [] -> Nothing
        ((p, _, _) : t) -> Just p

exit :: TriMesh -> Ray -> Maybe Vec3d
exit mesh r =
  case intersectTriMeshInside mesh r of
    Nothing -> Nothing -- traceStack ("(" ++ (show r) ++ "Point was not in mesh or didn't intersect...!?") Nothing
    Just p -> Just $ pointAtParameter r p

-- Hittests each tri in the mesh.  Should be absorbed into scene because scene could have better spacial data structure
hittestTriMesh :: (T.Triangle -> Ray -> Maybe Hit) -> TriMesh -> Ray -> Maybe Hit
hittestTriMesh f mesh ray@(Ray pos _) =
  let triIdxs = [0 .. VB.length (tris mesh) -1]
      hits = zipF triIdxs $ map (\tri -> f (indexAsScTri mesh tri) ray) triIdxs
      accum (_, h1) (_, h2) = closerHit pos h1 h2
   in case foldr (maybeCompare accum) Nothing hits of
        Nothing -> Nothing
        Just (i, h) ->
          Just MkHit
            { hitPos = hitPos h,
              hitInc = hitInc h,
              hitParam = hitParam h,
              hitNorm = hitNorm h,
              hitCoords = HitIndexed i $ hitCoords h
            }

-- This is wrong
insideTriMesh :: TriMesh -> Vec3d -> Bool
insideTriMesh mesh pt = foldl (&&) True $ map (\t -> T.insideTriPlane (indexAsScTri mesh t) pt) [0 .. VB.length (tris mesh) -1]
