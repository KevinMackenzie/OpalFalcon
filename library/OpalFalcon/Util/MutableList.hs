{-# LANGUAGE NoImplicitPrelude #-}

module OpalFalcon.Util.MutableList
  ( MList (),
    new,
    singleton,
    fromList,
    toList,
    readRef,
    empty,
    emptyRef,
    length,
    insertAfter,
    insertBefore,
    next,
    nextRef,
    prev,
    prevRef,
    erase,
    eraseRef,
    mapSlice,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Bool
import Data.Function
import Data.Functor
import Data.Int
import Data.STRef
import Prelude ((+), (==), undefined)

-- A mutable doubly-circularly-linked list of immutable elements
data MList s a
  = Node (STRef s (MList s a)) (STRef s (MList s a)) a
  | Null

{-# INLINE link #-}
link :: STRef s (MList s a) -> STRef s (MList s a) -> ST s ()
link n0 n1 =
  do
    modifySTRef n0 (\(Node p _ v) -> Node p n1 v)
    modifySTRef n1 (\(Node _ n v) -> Node n0 n v)

fromListHelper :: STRef s (MList s a) -> [a] -> ST s (STRef s (MList s a))
fromListHelper first [] = return first
fromListHelper first (h : t) =
  do
    curr <- singleton h
    nxt <- fromListHelper first t
    link curr nxt
    return curr

-- Creates a singleton list: O(1)
{-# INLINE singleton #-}
singleton :: a -> ST s (STRef s (MList s a))
singleton val =
  do
    n <- new
    initNull n val
    return n

-- Initializes a null reference to a singleton
{-# INLINE initNull #-}
initNull :: STRef s (MList s a) -> a -> ST s ()
initNull nRef val =
  writeSTRef nRef (Node nRef nRef val)

-- Constructs a new empty list: O(1)
{-# INLINE new #-}
new :: ST s (STRef s (MList s a))
new = newSTRef Null

-- Constructs the list: O(n)
fromList :: [a] -> ST s (STRef s (MList s a))
fromList [] = new
fromList (h : t) =
  do
    hd <- singleton h
    n <- fromListHelper hd t
    link hd n
    return hd

{-# INLINE readRef #-}
readRef :: STRef s (MList s a) -> ST s a
readRef nRef =
  do
    (Node _ _ a) <- readSTRef nRef
    return a

-- Gets the list starting at this node: O(n)
toList :: STRef s (MList s a) -> ST s [a]
toList nRef =
  let f r =
        do
          v <- readRef r
          n <- nextRef r
          if n == nRef
            then return $ [v]
            else do
              t <- f n
              return $ v : t
   in do
        e <- emptyRef nRef
        if e
          then return []
          else f nRef

-- Returns True if the list is empty: O(1)
{-# INLINE empty #-}
empty :: MList s a -> Bool
empty Null = True
empty (Node _ _ _) = False

-- Returns True if the list is empty: O(1)
{-# INLINE emptyRef #-}
emptyRef :: STRef s (MList s a) -> ST s Bool
emptyRef n = empty <$> (readSTRef n)

-- Returns the length of the list: O(n)
length :: STRef s (MList s a) -> ST s Int
length nRef =
  let f i r =
        do
          n <- nextRef r
          if n == nRef
            then return i
            else f (i + 1) n
   in do
        e <- emptyRef nRef
        if e
          then return 0
          else f 1 nRef

-- Inserts after the node: O(1)
{-# INLINE insertAfter #-}
insertAfter :: STRef s (MList s a) -> a -> ST s ()
insertAfter nRef val =
  do
    node <- readSTRef nRef
    case node of
      Null -> initNull nRef val
      (Node _ nxt _) -> do
        newNode <- singleton val
        link nRef newNode
        link newNode nxt

-- Inserts before the node: O(1)
{-# INLINE insertBefore #-}
insertBefore :: STRef s (MList s a) -> a -> ST s ()
insertBefore nRef val =
  do
    node <- readSTRef nRef
    case node of
      Null -> initNull nRef val
      (Node prv _ _) -> do
        newNode <- singleton val
        link prv newNode
        link newNode nRef

-- Gets the next node: O(1)
-- NOTE: List must not be empty
{-# INLINE next #-}
next :: MList s a -> STRef s (MList s a)
next Null = undefined
next (Node _ n _) = n

-- Gets the next node: O(1)
-- NOTE: List must not be empty
{-# INLINE nextRef #-}
nextRef :: STRef s (MList s a) -> ST s (STRef s (MList s a))
nextRef nRef = next <$> (readSTRef nRef)

-- Gets the previous node: O(1)
-- NOTE: List must not be empty
{-# INLINE prev #-}
prev :: MList s a -> STRef s (MList s a)
prev Null = undefined
prev (Node p _ _) = p

-- Gets the previous node: O(1)
-- NOTE: List must not be empty
{-# INLINE prevRef #-}
prevRef :: STRef s (MList s a) -> ST s (STRef s (MList s a))
prevRef nRef = prev <$> (readSTRef nRef)

-- removes the node O(1)
-- NOTE: List must not be empty
-- NOTE: invalidates the parameter
{-# INLINE erase #-}
erase :: STRef s (MList s a) -> ST s (a, STRef s (MList s a))
erase nRef =
  do
    (Node prv nxt v) <- readSTRef nRef
    if prv == nxt
      then do
        writeSTRef nRef Null
        return (v, nRef)
      else do
        link prv nxt
        writeSTRef nRef Null
        return (v, nxt)

eraseRef :: STRef s (STRef s (MList s a)) -> ST s a
eraseRef nRefRef =
  do
    nRef <- readSTRef nRefRef
    (a, nRef') <- erase nRef
    writeSTRef nRefRef nRef'
    return a

mapSlice :: (a -> b) -> STRef s (MList s a) -> STRef s (MList s a) -> ST s [b]
mapSlice f begin end =
  if begin == end
    then return []
    else do
      v <- readRef begin
      n <- nextRef begin
      (f v :) <$> (mapSlice f n end)
