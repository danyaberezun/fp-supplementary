module NaiveQueue (NaiveQueue) where
import Prelude hiding (head, tail)
import Queue

data NaiveQueue a = NQ [a] [a]

-- invariant: f may become empty iff list r is also empty
inv (NQ [] r) = NQ (reverse r) []
inv q = q

instance Queue NaiveQueue where
  empty  = NQ [] []
  isEmpty (NQ []   [])   = True
  isEmpty (NQ _     _)   = False
  enqueue (NQ f     r) x = inv $ NQ f (x:r)
  head    (NQ (x:_) _)   = x
  tail    (NQ (_:f) r)   = inv $ NQ f r
