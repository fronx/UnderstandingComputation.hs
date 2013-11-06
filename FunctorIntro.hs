module FunctorIntro where

import Prelude hiding ( Functor
                      , fmap
                      , Maybe, Just, Nothing
                      )

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
data Maybe a = Nothing | Just a deriving Show

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f ma = case ma of
    Just x  -> Just (f x)
    Nothing -> Nothing

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node ta tb) = Node (fmap f ta) (fmap f tb)

tree1 = Node (Node (Leaf 3)
                   (Node (Leaf 4) (Leaf 5)))
             (Leaf 6)

double x = 2 * x

doublefap :: Functor f => (a -> a) -> f a -> f a
doublefap f fa = fmap f (fmap f fa)


main = do
  print $ fmap double tree1
  print $ fmap double [ 7, 23, 64 ]
  print $ fmap double $ Just 9
  print $ fmap double Nothing
  print $ doublefap double tree1
  print $ doublefap double $ Just 9
