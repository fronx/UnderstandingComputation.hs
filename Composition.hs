module Composition where

import Prelude hiding ( Functor
                      , fmap
                      , Compose
                      , Maybe (..)
                      )
import FunctorIntro

data Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

tree1 = Node (Node (Leaf [1,2,3])
                   (Node (Leaf [4,5]) (Leaf [6,7])))
             (Leaf [8])

main = do
  print $ fmap double $ Compose [Just 1, Nothing, Just 3]
  print $ fmap double $ Compose tree1
