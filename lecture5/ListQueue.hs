module ListQueue (ListQueue) where
import Prelude hiding (head, tail)
import Queue

data ListQueue a = LQ [a]

instance Queue ListQueue where
  empty  = LQ []
  isEmpty (LQ [])    = True
  isEmpty (LQ _ )    = False
  enqueue (LQ l) x   = LQ (l++[x])
  head    (LQ (x:_)) = x
  tail    (LQ (_:f)) = LQ f
