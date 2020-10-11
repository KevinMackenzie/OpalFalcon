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
repeatMF 0 f = return []
repeatMF c f = do
  v <- f
  (v :) <$> (repeatMF (c -1) f)
