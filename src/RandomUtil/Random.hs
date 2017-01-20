module RandomUtil.Random
  (
    randomMap
  , randomMapTwoGens
  ) where

import System.Random

randomMap :: RandomGen g => (g -> a -> (b,g)) -> g -> [a] -> ([b],g)
randomMap _ g []     = ([], g)
randomMap f g (x:xs) = let (y,g') = f g x
                           (ys, gf) = randomMap f g' xs
                       in (y:ys, gf)

randomMapTwoGens :: RandomGen g => (g -> g -> a -> (b,(g,g))) -> g -> g -> [a] -> ([b], (g,g))
randomMapTwoGens _ g1 g2 []     = ([], (g1, g2))
randomMaptwoGens f g1 g2 (x:xs) = let (y, (g1', g2'))  = f g1 g2 x
                                      (ys, gs) = randomMapTwoGens f g1' g2' xs
                                  in (y:ys, gs)
