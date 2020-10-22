module OpalFalcon.Util.Misc where

-- Filters the Nothing values out of a list
filterJust :: [Maybe a] -> [a]
filterJust [] = []
filterJust ((Nothing) : xs) = filterJust xs
filterJust ((Just x) : xs) = x : (filterJust xs)

interleave :: [a] -> [a] -> [a]
interleave l r =
  let fl [] r = r
      fl l [] = l
      fl (h : t) r = h : (fr t r)
      fr l [] = l
      fr [] r = r
      fr l (h : t) = h : (fl l t)
   in fl l r

repeatMF :: (Monad m) => Integer -> m a -> m [a]
repeatMF 0 _ = return []
repeatMF c f = do
  v <- f
  (v :) <$> (repeatMF (c -1) f)

maybeCompare :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe a
maybeCompare f m0 m1 =
  case (m0, m1) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just v) -> Just v
    (Just v, Nothing) -> Just v
    (Just h0, Just h1) -> if f h0 h1 then Just h0 else Just h1

tuple :: a -> b -> (a, b)
tuple x y = (x, y)

zipF :: Functor f => [a] -> [f b] -> [f (a, b)]
zipF [] _ = []
zipF _ [] = []
zipF (h1 : t1) (h2 : t2) = ((tuple h1) <$> h2) : (zipF t1 t2)
