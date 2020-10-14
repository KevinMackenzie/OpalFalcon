{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module OpalFalcon.Util.StrictList where

data List a
  = !a :! !(List a)
  | B
  deriving (Eq, Show, Read, Foldable, Traversable, Functor)

fromList :: [a] -> List a
fromList [] = B
fromList (h : t) = h :! (fromList t)

toList :: List a -> [a]
toList B = []
toList (h :! t) = h : (toList t)

forceList :: [a] -> [a]
forceList = toList . fromList