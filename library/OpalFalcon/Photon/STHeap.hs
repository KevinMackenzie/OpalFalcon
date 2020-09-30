{-# LANGUAGE RankNTypes #-}

module OpalFalcon.Photon.STHeap
  ( STHeap,
    mkSTHeap,
    pushHeap,
    getHeapRoot,
    getHeapRootPriority,
    getHeapContents,
    getHeapSize,
  )
where

import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Storable (Storable)

-- TODO: replace all vector ops with unsafe ones once we know it is stable

{-# INLINE readOneBased #-}
readOneBased :: (PrimMonad m, Storable a) => VSM.MVector (PrimState m) a -> Int -> m a
readOneBased vec idx = VSM.read vec (idx -1)

{-# INLINE writeOneBased #-}
writeOneBased :: (PrimMonad m, Storable a) => VSM.MVector (PrimState m) a -> Int -> a -> m ()
writeOneBased vec idx = VSM.write vec (idx -1)

{-# INLINE swapOneBased #-}
swapOneBased :: (PrimMonad m, Storable a) => VSM.MVector (PrimState m) a -> Int -> Int -> m ()
swapOneBased vec idx0 idx1 = VSM.swap vec (idx0 -1) (idx1 -1)

-- This is a max-heap
data STHeap s e p = STHeap (e -> p) Int (STRef s Int) (VSM.MVector s e)

mkSTHeap :: (Storable e, Ord p) => Int -> e -> (e -> p) -> ST s (STHeap s e p)
mkSTHeap size blank priority = do
  sz <- newSTRef 0
  vec <- VSM.new size
  return $ STHeap priority size sz vec

percolateUp :: (Storable e, Ord p) => Int -> STHeap s e p -> ST s ()
percolateUp 0 _ = return ()
percolateUp 1 _ = return ()
percolateUp p h@(STHeap pr _ sz vec) =
  let parentIdx = p `div` 2
   in do
        val <- readOneBased vec p
        pval <- readOneBased vec parentIdx
        when ((pr val) > (pr pval)) $ do
          swapOneBased vec p parentIdx
          percolateUp parentIdx h

-- NOTE: only call this on a full heap
percolateDown :: (Storable e, Ord p) => Int -> STHeap s e p -> ST s ()
percolateDown p h@(STHeap pr cap _ vec) =
  let lChild = 2 * p
      rChild = 2 * p + 1
   in if lChild > cap
        then return ()
        else do
          pVal <- readOneBased vec p
          lVal <- readOneBased vec lChild
          (cIdx, cVal) <-
            if rChild > cap
              then return (lChild, lVal)
              else do
                rVal <- readOneBased vec rChild
                return $
                  if (pr lVal) > (pr rVal)
                    then (lChild, lVal)
                    else (rChild, rVal)
          when ((pr cVal) > (pr pVal)) $ do
            swapOneBased vec p cIdx
            percolateDown cIdx h

-- replaces highest priority item
pushHeap :: (Storable e, Ord p) => e -> STHeap s e p -> ST s ()
pushHeap val h@(STHeap pr cap sz vec) = do
  size <- readSTRef sz
  if (size < cap)
    then do
      -- Until we hit capacity, fill top-down
      modifySTRef sz (+ 1)
      writeOneBased vec (size + 1) val
      percolateUp (size + 1) h
    else do
      -- Once we hit capacity, replace the highest priority item
      hval <- readOneBased vec 1
      when ((pr val) < (pr hval)) $ do
        writeOneBased vec 1 val
        percolateDown 1 h

getHeapRoot :: (Storable e, Ord p) => STHeap s e p -> ST s e
getHeapRoot (STHeap _ _ _ vec) = readOneBased vec 1

getHeapRootPriority :: (Storable e, Ord p) => STHeap s e p -> ST s p
getHeapRootPriority h@(STHeap pr _ _ _) = (getHeapRoot h) >>= (return . pr)

getHeapSize :: Storable e => STHeap s e p -> ST s Int
getHeapSize (STHeap _ _ sz _) = readSTRef sz

getHeapContents :: (Storable e) => STHeap s e p -> ST s (VS.Vector e)
getHeapContents (STHeap pr cap sz vec) = do
  vecImmutable <- VS.freeze vec
  numElems <- readSTRef sz
  return $ VS.take numElems vecImmutable
