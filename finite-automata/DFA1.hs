module DFA1 where

import Data.Maybe
import Data.List

data Node id = Node id
data Transition id trans = Transition (Node id) trans (Node id)
type Machine    id trans = [Transition id trans]

getFromId :: Transition id t -> id
getFromId (Transition (Node a) _ _) = a

getToId :: Transition id t -> id
getToId (Transition _ _ (Node b)) = b

getTrans :: Transition id t -> t
getTrans (Transition _ x _) = x

matchTransition :: Int -> Char -> Transition Int Char -> Bool
matchTransition a x trans =
  (getFromId trans == a) && (getTrans trans == x)

nextState :: Machine Int Char -> Int -> Char -> Maybe Int
nextState machine nodeId trans =
  case match of
    Just trans -> Just $ getToId trans
    Nothing    -> Nothing
  where match = find (matchTransition nodeId trans) machine

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
