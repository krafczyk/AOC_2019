module Library.Maze where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Library.Vec
import Data.List

class (Eq a, VecI a, Ord a) => MappableI a where
    neighbors :: a -> [a]

distMapImp :: (MappableI a) => (a -> Bool) -> (Map.Map a Int,[a]) -> (Map.Map a Int,[a])
distMapImp validator (dmap, que) =
    (newDmap, newQue)
    where next = head que
          q1 = drop 1 que
          val_n = filter (validator) $ neighbors next
          from_cand = filter (\p -> p `elem` (Map.keys dmap)) val_n
          from = head $ sortBy (\p1 p2 -> compare (dmap Map.! p1) (dmap Map.! p2)) from_cand
          newDmap = Map.insert next ((dmap Map.! from)+1) dmap
          not_vis_n = filter (\p -> not $ p `elem` (Map.keys dmap)) val_n
          new_n = filter (\p -> not $ p `elem` q1) not_vis_n
          newQue = q1 ++ new_n

distMap :: (MappableI a) => Set.Set a -> a -> Map.Map a Int
distMap map orig =
    distMapBy (\p -> True) map orig

distMapBy :: (MappableI a) => (a -> Bool) -> Set.Set a -> a -> Map.Map a Int
distMapBy validator map orig =
    fst $ head $ dropWhile (\(_,q) -> not $ null q) $ iterate (distMapImp (is_valid)) (init_dmap, init_queue)
    where is_valid = \p -> (Set.member p map) && (validator p)
          init_dmap = Map.singleton orig 0
          init_queue = filter (is_valid) $ neighbors orig
