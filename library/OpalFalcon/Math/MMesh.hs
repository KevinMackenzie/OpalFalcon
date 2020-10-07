module OpalFalcon.Math.MMesh
  ( MutableMesh (vertices),
    Tri,
    triTuple,
    mkTri,
    mkTriT,
    triEdges,
    otherTriEdges,
    new,
    freezeEdges,
    freezePolys,
    idxVerts,
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
import qualified Data.Set as Set
import qualified Data.Vector as VB
import qualified Data.Vector.Mutable as VBM
import qualified Data.Vector.Storable as VS
import OpalFalcon.Math.Vector

data Tri = Tri !Int !Int !Int deriving (Eq, Show, Read)

{-# INLINE triTuple #-}
triTuple :: Tri -> (Int, Int, Int)
triTuple (Tri a b c) = (a, b, c)

-- Automatically rotates triangle to lowest-index-first ordering
{-# INLINE mkTriT #-}
mkTriT :: (Int, Int, Int) -> Tri
mkTriT (a, b, c) = mkTri a b c

{-# INLINE mkTri #-}
mkTri :: Int -> Int -> Int -> Tri
mkTri a b c =
  if a < b
    then
      if a < c
        then Tri a b c
        else Tri c a b
    else
      if b > c
        then Tri c a b
        else Tri b c a

{-# INLINE triCmp #-}
triCmp :: Tri -> Tri -> Ordering
triCmp (Tri a1 b1 c1) (Tri a2 b2 c2) =
  case compare a1 a2 of
    GT -> GT
    LT -> LT
    EQ -> case compare b1 b2 of
      GT -> GT
      LT -> LT
      EQ -> compare c1 c2

instance Ord Tri where
  {-# INLINE compare #-}
  compare = triCmp

{-# INLINE otherTriEdges #-}
otherTriEdges :: Tri -> Int -> [(Int, Int)]
otherTriEdges (Tri a b c) p0 =
  let e0 = (a, b)
      e1 = (b, c)
      e2 = (c, a)
   in if p0 == a then [e1, e2] else if p0 == b then [e2, e0] else [e0, e1]

{-# INLINE triEdges #-}
triEdges :: Tri -> [(Int, Int)]
triEdges (Tri a b c) =
  let e0 = (a, b)
      e1 = (b, c)
      e2 = (c, a)
   in [e0, e1, e2]

-- Mutable in topological interpretation (edges, faces)
-- Immutable in vertices
-- Note: Edges / polys will be consistent
-- Note: Does not enforce manifold conditions
-- Note: Runtime notation: k = vertex degree; N = # of vertices
data MutableMesh s
  = MM
      { -- Mesh vertices; immutable
        vertices :: VS.Vector Vec3d,
        -- Mapping from vertex index to connected vertices; Can be duplicates if multiple polys share edge
        edges :: VB.MVector s [Int],
        -- Mapping from vertex index to connected polygons
        polys :: VB.MVector s [(Int, Int)]
      }

-- O(N)
new :: VS.Vector Vec3d -> ST s (MutableMesh s)
new vs =
  let vcount = VS.length vs
   in do
        e <- VBM.replicate vcount []
        p <- VBM.replicate vcount []
        return $ MM {vertices = vs, edges = e, polys = p}

freezeEdges :: MutableMesh s -> ST s (VB.Vector (VS.Vector Int))
freezeEdges mesh =
  do
    e <- VB.freeze (edges mesh)
    return $ VB.map VS.fromList e

freezePolys :: MutableMesh s -> ST s (VB.Vector (VB.Vector (Int, Int)))
freezePolys mesh =
  do
    p <- VB.freeze (polys mesh)
    return $ VB.map VB.fromList p

{-# INLINE idxVerts #-}
idxVerts :: MutableMesh s -> Int -> Vec3d
idxVerts mesh idx = (vertices mesh) VS.! idx

-- O(1)
addTriUnsafe :: MutableMesh s -> Tri -> ST s ()
addTriUnsafe mesh (Tri p0 p1 p2) =
  do
    VBM.modify (polys mesh) ((p1, p2) :) p0
    VBM.modify (polys mesh) ((p2, p0) :) p1
    VBM.modify (polys mesh) ((p0, p1) :) p2
    VBM.modify (edges mesh) ((p1 :) . (p2 :)) p0
    VBM.modify (edges mesh) ((p2 :) . (p0 :)) p1
    VBM.modify (edges mesh) ((p0 :) . (p1 :)) p2

-- O(k)
addTri :: MutableMesh s -> Tri -> ST s Bool
addTri mesh tri =
  do
    cond <- not <$> (hasTri mesh tri)
    when cond $ addTriUnsafe mesh tri
    return cond

-- O(k)
removeTriUnsafe :: MutableMesh s -> Tri -> ST s ()
removeTriUnsafe mesh (Tri p0 p1 p2) =
  do
    VBM.modify (polys mesh) (delete (p1, p2)) p0
    VBM.modify (polys mesh) (delete (p2, p0)) p1
    VBM.modify (polys mesh) (delete (p0, p1)) p2
    VBM.modify (edges mesh) (\\ [p1, p2]) p0
    VBM.modify (edges mesh) (\\ [p2, p0]) p1
    VBM.modify (edges mesh) (\\ [p0, p1]) p2

-- O(k)
removeTri :: MutableMesh s -> Tri -> ST s Bool
removeTri mesh tri =
  do
    cond <- hasTri mesh tri
    when cond $ removeTriUnsafe mesh tri
    return cond

-- O(k)
{-# INLINE hasTri #-}
hasTri :: MutableMesh s -> Tri -> ST s Bool
hasTri mesh (Tri p0 p1 p2) =
  do
    poly <- VBM.read (polys mesh) p0
    return ((p1, p2) `elem` poly)

-- O(kN + NlogN)
-- TODO: This could be improved to O(N) if DFS is used and we assume a single surface
enumerateTris :: MutableMesh s -> ST s [Tri]
enumerateTris mesh =
  do
    allPolys <-
      mapM
        ( \x ->
            do
              ps <- VBM.read (polys mesh) x
              return $ map (\(e1, e2) -> mkTri x e1 e2) ps
        )
        [0 .. (VS.length $ vertices mesh) -1]
    return $ Set.toList $ Set.fromList (concat allPolys)

-- O(k^2)
removeConnectedTris :: MutableMesh s -> Int -> ST s [Tri]
removeConnectedTris mesh pt =
  do
    tris <- (map (\(p1, p2) -> mkTri pt p1 p2)) <$> VBM.read (polys mesh) pt
    _ <- mapM (\tri -> removeTriUnsafe mesh tri) tris
    return tris

-- O(1)
{-# INLINE getTriPoints #-}
getTriPoints :: MutableMesh s -> Tri -> (Vec3d, Vec3d, Vec3d)
getTriPoints (MM {vertices = verts}) (Tri p0 p1 p2) =
  let v0 = verts VS.! p0
      v1 = verts VS.! p1
      v2 = verts VS.! p2
   in (v0, v1, v2)

-- e2 must be counter-clockwise on the edge from the point ps1 originates from
unionPolys :: Int -> [(Int, Int)] -> [Int]
unionPolys e2 ps1 = map fst $ filter (\(_, p) -> p == e2) ps1

-- O(k)
-- NOTE: Assumes manifold conditions locally
crossEdge :: MutableMesh s -> Int -> Int -> ST s (Maybe Int)
crossEdge mesh e1 e2 =
  do
    ps1 <- VBM.read (polys mesh) e1
    return $ case unionPolys e2 ps1 of
      [p] -> Just p
      _ -> Nothing
