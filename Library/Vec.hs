module Library.Vec where

class VecI a where
    dist :: a -> a -> Int
    dot :: a -> a -> Int
    mag :: a -> Int
