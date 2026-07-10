module InstanceGraft where

combine :: Int -> Int -> Int
combine a b = a
  + b

class C t where
  go :: t -> Int
  go n = todo

instance C Int where
  go n = todo
