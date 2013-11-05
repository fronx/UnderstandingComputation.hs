module DFA1 where

import Data.Maybe

data Node id = Node id
data Transition id trans = Transition (Node id) trans (Node id)
type Machine    id trans = [Transition id trans]

matchTransition :: Int -> Char -> Transition Int Char -> Bool
matchTransition _a _x (Transition (Node a) x (Node b)) =
  (a == _a) && (x == _x)

nextState :: Machine Int Char -> Int -> Char -> Maybe Int
nextState machine nodeId trans =
  case match of
    Just (Transition _ _ (Node b)) -> Just b
    Nothing -> Nothing
  where match = listToMaybe $
                take 1 $
                filter (matchTransition nodeId trans) machine

machine1 =
  [ Transition (Node 1) 'a' (Node 2)
  , Transition (Node 1) 'b' (Node 1)
  , Transition (Node 2) 'a' (Node 2)
  , Transition (Node 2) 'b' (Node 3)
  , Transition (Node 3) 'a' (Node 3)
  , Transition (Node 3) 'b' (Node 3)
  ]

main = do
  print $ nextState machine1 1 'a'
  print $ nextState machine1 1 'b'
  print $ nextState machine1 2 'b'
