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
import Control.Monad.ST
import qualified Data.Array
import Data.Array.ST
import Data.STRef

-- TODO: This is a useful thing to have, but it is mostly a stop-gap until a more performant priority heap that operates on unboxed photons and avoids lifting until final density estimations are required

-- This is a max-heap
data STHeap s e p = STHeap (e -> p) Int (STRef s Int) (STArray s Int e)

mkSTHeap :: (Ord p) => Int -> e -> (e -> p) -> ST s (STHeap s e p)
mkSTHeap size blank priority = do
  sz <- newSTRef 0
  arr <- newArray (1, size) blank
  return $ STHeap priority size sz arr

percolateUp :: (Ord p) => Int -> STHeap s e p -> ST s ()
percolateUp 0 _ = return ()
percolateUp 1 _ = return ()
percolateUp p h@(STHeap pr _ sz arr) =
  let parentIdx = p `div` 2
   in do
        val <- readArray arr p
        pval <- readArray arr parentIdx
        when ((pr val) > (pr pval)) $ do
          writeArray arr parentIdx val
          writeArray arr p pval
          percolateUp parentIdx h

-- NOTE: only call this on a full heap
percolateDown :: (Ord p) => Int -> STHeap s e p -> ST s ()
percolateDown p h@(STHeap pr cap _ arr) =
  let lChild = 2 * p
      rChild = 2 * p + 1
   in if lChild > cap
        then return ()
        else do
          pVal <- readArray arr p
          lVal <- readArray arr lChild
          (cIdx, cVal) <-
            if rChild > cap
              then return (lChild, lVal)
              else do
                rVal <- readArray arr rChild
                return $
                  if (pr lVal) > (pr rVal)
                    then (lChild, lVal)
                    else (rChild, rVal)
          when ((pr cVal) > (pr pVal)) $ do
            writeArray arr p cVal
            writeArray arr cIdx pVal
            percolateDown cIdx h

-- replaces highest priority item
pushHeap :: (Ord p) => e -> STHeap s e p -> ST s ()
pushHeap val h@(STHeap pr cap sz arr) = do
  size <- readSTRef sz
  if (size < cap)
    then do
      -- Until we hit capacity, fill top-down
      modifySTRef sz (+ 1)
      writeArray arr (size + 1) val
      percolateUp (size + 1) h
    else do
      -- Once we hit capacity, replace the highest priority item
      hval <- readArray arr 1
      when ((pr val) < (pr hval)) $ do
        writeArray arr 1 val
        percolateDown 1 h

getHeapRoot :: (Ord p) => STHeap s e p -> ST s e
getHeapRoot (STHeap _ _ _ arr) = readArray arr 1

getHeapRootPriority :: (Ord p) => STHeap s e p -> ST s p
getHeapRootPriority h@(STHeap pr _ _ _) = (getHeapRoot h) >>= (return . pr)

getHeapSize :: STHeap s e p -> ST s Int
getHeapSize (STHeap _ _ sz _) = readSTRef sz

getHeapContents :: STHeap s e p -> ST s ([e], Int)
getHeapContents (STHeap pr cap sz arr) = do
  arrElems <- getElems arr
  numElems <- readSTRef sz
  return (take numElems arrElems, numElems)

runSTHeap :: (forall s. ST s (STHeap s e p)) -> [e]
runSTHeap action = Data.Array.elems $ runSTArray $ (\(STHeap _ _ _ arr) -> arr) <$> action
