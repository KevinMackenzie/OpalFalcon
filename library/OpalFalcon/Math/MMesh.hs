module OpalFalcon.Math.MMesh
  ( MutableMesh (..),
    new,
    addTri,
    removeTri,
    hasTri,
    enumerateTris,
    removeConnectedTris,
    crossEdge,
    getTriPoints,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.List
import qualified Data.Vector as VB
import qualified Data.Vector.Mutable as VBM
import qualified Data.Vector.Storable as VS
import OpalFalcon.Math.Vector

-- Mutable in topological interpretation (edges, faces); cannot add / remove vertices
-- Note: Edges / polys will be consistent
data MutableMesh s
  = MM
      { -- Mesh vertices; immutable
        vertices :: VS.Vector Vec3d,
        -- Mapping from vertex index to connected vertices; Can be duplicates if multiple polys share edge
        edges :: VB.MVector s [Int],
        -- Mapping from vertex index to connected polygons
        polys :: VB.MVector s [(Int, Int)]
      }

new :: VS.Vector Vec3d -> ST s (MutableMesh s)
new vs =
  let vcount = VS.length vs
   in do
        e <- VBM.replicate vcount []
        p <- VBM.replicate vcount []
        return $ MM {vertices = vs, edges = e, polys = p}

addTriUnsafe :: MutableMesh s -> Int -> Int -> Int -> ST s ()
addTriUnsafe mesh p0 p1 p2 =
  do
    VBM.modify (polys mesh) ((p1, p2) :) p0
    VBM.modify (polys mesh) ((p2, p0) :) p1
    VBM.modify (polys mesh) ((p0, p1) :) p2
    VBM.modify (edges mesh) ((p1 :) . (p2 :)) p0
    VBM.modify (edges mesh) ((p2 :) . (p0 :)) p1
    VBM.modify (edges mesh) ((p0 :) . (p1 :)) p2

addTri :: MutableMesh s -> Int -> Int -> Int -> ST s Bool
addTri mesh p0 p1 p2 =
  do
    cond <- not <$> (hasTri mesh p0 p1 p2)
    when cond $ addTriUnsafe mesh p0 p1 p2
    return cond

removeTriUnsafe :: MutableMesh s -> Int -> Int -> Int -> ST s ()
removeTriUnsafe mesh p0 p1 p2 =
  do
    VBM.modify (polys mesh) (delete (p1, p2)) p0
    VBM.modify (polys mesh) (delete (p2, p0)) p1
    VBM.modify (polys mesh) (delete (p0, p1)) p2
    VBM.modify (edges mesh) (\\ [p1, p2]) p0
    VBM.modify (edges mesh) (\\ [p2, p0]) p1
    VBM.modify (edges mesh) (\\ [p0, p1]) p2

removeTri :: MutableMesh s -> Int -> Int -> Int -> ST s Bool
removeTri mesh p0 p1 p2 =
  do
    cond <- hasTri mesh p0 p1 p2
    when cond $ removeTriUnsafe mesh p0 p1 p2
    return cond

hasTri :: MutableMesh s -> Int -> Int -> Int -> ST s Bool
hasTri mesh p0 p1 p2 =
  do
    poly <- VBM.read (polys mesh) p0
    return ((p1, p2) `elem` poly)

enumerateTris :: MutableMesh s -> ST s [(Int, Int, Int)]
enumerateTris _ = undefined

removeConnectedTris :: MutableMesh s -> Int -> ST s [(Int, Int)]
removeConnectedTris mesh pt =
  do
    ps <- VBM.read (polys mesh) pt
    _ <- mapM (\(p1, p2) -> removeTriUnsafe mesh pt p1 p2) ps
    return ps

getTriPoints :: MutableMesh s -> Int -> Int -> Int -> (Vec3d, Vec3d, Vec3d)
getTriPoints (MM {vertices = verts}) p0 p1 p2 =
  let v0 = verts VS.! p0
      v1 = verts VS.! p1
      v2 = verts VS.! p2
   in (v0, v1, v2)

-- e2 must be counter-clockwise on the edge from the point ps1 originates from
unionPolys :: Int -> [(Int, Int)] -> [Int]
unionPolys e2 ps1 = map fst $ filter (\(_, p) -> p == e2) ps1

-- NOTE: Assumes manifold conditions locally
crossEdge :: MutableMesh s -> Int -> Int -> ST s (Maybe Int)
crossEdge mesh e1 e2 =
  do
    ps1 <- VBM.read (polys mesh) e1
    return $ case unionPolys e2 ps1 of
      [p] -> Just p
      _ -> Nothing
