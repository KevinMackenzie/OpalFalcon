module OpalFalcon.KdTree
  ( AxisAlignedBoundingBox (..),
    KdTree (..),
    BoxedKdTree,
    KdTreeObject (..),
    KdAxis (..),
    KdTreeTree (..),
    balance,
    findAABB,
    mkKdTree,
    mkKdTree1,
    kdTreeElems,
    sortByDim,
    splitPoints,
    kdTreeSize,
    axisElem
  )
where

-- import Data.Ix
import Data.Array.Unboxed
import Data.List (sortBy)
import OpalFalcon.Math.Vector

class KdTreeObject a where

  blank :: a

  pos :: a -> Vec3d

  setAxis :: a -> KdAxis -> a

-- Helper type to build the tree
data KdAxis = XAxis | YAxis | ZAxis deriving (Eq, Show)

data KdTreeTree d = KdNode KdAxis d (KdTreeTree d) (KdTreeTree d) | KdNull deriving (Eq)

printKdTree pfx (KdNull) = ""
printKdTree pfx (KdNode ax v l r) = "\n" ++ pfx ++ "KdNode(" ++ (show ax) ++ ") " ++ (show v) ++ (printKdTree (pfx ++ "  ") l) ++ (printKdTree (pfx ++ "  ") r)

instance Show a => Show (KdTreeTree a) where
  show = printKdTree ""

-- Box position with half-sizes in each dimmension
data AxisAlignedBoundingBox = AABB Vec3d Vec3d deriving (Eq, Show)

findAABB :: KdTreeObject b => [b] -> AxisAlignedBoundingBox
findAABB [] = AABB origin origin
findAABB (x : []) = AABB (pos x) origin
findAABB (l : ls) =
  let extrema =
        foldl
          ( \(V3 (xMin, xMax) (yMin, yMax) (zMin, zMax)) (V3 x y z) ->
              V3
                (min x xMin, max x xMax)
                (min y yMin, max y yMax)
                (min z zMin, max z zMax)
          )
          (fmap (\x -> (x, x)) $ pos l)
          (map pos ls)
   in AABB (fmap (\(mn, mx) -> (mn + mx) / 2) extrema) (fmap (\(mn, mx) -> (mx - mn) / 2) extrema)

largestDimmension :: AxisAlignedBoundingBox -> KdAxis
largestDimmension (AABB _ (V3 xs ys zs)) =
  if xs > ys
    then if xs > zs then XAxis else ZAxis
    else if ys > zs then YAxis else ZAxis

axisElem :: KdAxis -> Vec3 a -> a
axisElem XAxis (V3 v _ _) = v
axisElem YAxis (V3 _ v _) = v
axisElem ZAxis (V3 _ _ v) = v

sortByFunc :: KdTreeObject a => KdAxis -> a -> a -> Ordering
sortByFunc XAxis o0 o1 = compare (xPos $ pos o0) (xPos $ pos o1)
sortByFunc YAxis o0 o1 = compare (yPos $ pos o0) (yPos $ pos o1)
sortByFunc ZAxis o0 o1 = compare (zPos $ pos o0) (zPos $ pos o1)

sortByDim :: KdTreeObject a => KdAxis -> [a] -> [a]
sortByDim dim points = sortBy (sortByFunc dim) points

splitPoints :: KdTreeObject a => KdAxis -> [a] -> (Maybe a, [a], [a])
splitPoints _ [] = (Nothing, [], [])
splitPoints dim points =
  let (lf, lbh : lbt) = splitAt ((length points) `div` 2) $ sortByDim dim points
   in (Just lbh, lf, lbt)

balance :: KdTreeObject a => [a] -> KdTreeTree a
balance points =
  if length points == 0
    then KdNull
    else
      let dim = largestDimmension $ findAABB points
          (med, l, r) = splitPoints dim points
       in case med of
            Just m -> KdNode dim m (balance l) (balance r)
            Nothing -> KdNull

-- gets the tree as a list in heap-order (level-order)
treeToList :: KdTreeObject a => KdTreeTree a -> [a]
treeToList kd = f kd b []
  where
    f (KdNode ax x l r) fw bw = (setAxis x ax) : fw ([l, r] : bw)
    f KdNull fw bw = fw bw
    b [] = []
    b qs = foldl (foldr f) b qs []

-- An immutable 3d kd tree with internal array representation
--      array type ; Index type ; data type
data KdTree arr ix d = KdTree (arr ix d) deriving (Show)

type BoxedKdTree e = KdTree Array Int e

mkKdTree :: (IArray arr a, KdTreeObject a) => [a] -> KdTree arr Int a
mkKdTree l = KdTree $ listArray (1 :: Int, length l) $ treeToList $ balance l

mkKdTree1 :: (IArray arr a, KdTreeObject a) => [a] -> KdTree arr Int a
mkKdTree1 l = KdTree $ listArray (1 :: Int, length l) $ l

kdTreeElems :: (IArray arr a, Ix i) => KdTree arr i a -> [a]
kdTreeElems (KdTree a) = elems a

kdTreeSize :: (IArray arr a, Ix i) => KdTree arr i a -> i
kdTreeSize (KdTree a) =
  let (_, l) = bounds a
   in l
