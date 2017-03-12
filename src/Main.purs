module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Set (empty, insert, member)

data Coords = Coords Int Int
-- class (Eq a) <= Ord a where
-- instance eqCoords :: Eq Coords where
--   eq (Coords ax ay) (Coords bx by) = ax == bx && ay == by
derive instance eqCoords :: Eq Coords
-- compare :: a -> a -> Ordering
-- instance ordCoords :: Ord Coords where
--   compare (Coords ax ay) (Coords bx by) =
--     compare (compare ax bx) (compare ay by)
derive instance ordCoords :: Ord Coords

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  logShow $ eq (Coords 1 1) (Coords 1 1)
  logShow $ compare 1 2
  logShow $ compare 2 1
  logShow $ compare 2 2
  logShow $ compare LT EQ
  logShow $ compare (Coords 1 1) (Coords 1 2)
  logShow $ compare EQ LT
  logShow $ member (Coords 1 1) set
  logShow $ member (Coords 1 1) set2
  logShow $ member (Coords 1 1) set3
  logShow $ member (Coords 1 2) set3
  where
    set = empty
    set2 = insert (Coords 1 1) set
    set3 = insert (Coords 1 2) set2
