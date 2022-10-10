module RealTimeQueue (RealTimeQueue) where
import Prelude hiding (head, tail)
import Queue

data RealTimeQueue a = RTQ [a] [a] [a]

--- Specification: rotate f r a = f ++ reverse(r) ++ a
--- rotate($Nil, $Cons(y, $Nil), a) ≡ $Nil ++ reverse($Cons(y, $Nil)) ++ a ≡ $Cons(y,a)
rotate []     (y:_ ) a = y : a
--- rotate($Cons(x,f), $Cons(y,r), a) ≡ $Cons(x,f) ++ reverse($Cons(y,r)) ++ a
---    ≡ $Cons(x,f) ++ reverse($Cons(y,r)) ++ a
---    ≡ $Cons(x,f) ++ reverse(r) ++ $Cons(y,a))
---    ≡ $Cons(x,rotate (f, r, $Cons(y,a)))
rotate (x:xs) (y:ys) a = x : (rotate xs ys (y:a))

--- forcing one step of ``reverce''
---    by pattern-matching on accumulator and forgetting its head
---    Note: forcing accumulator = forcing front
exec f r (x:s) = RTQ f  r  s
--- reorder the queue when |r| = |f| + 1 (i.e. |a| = 0)
exec f r []    = RTQ f' [] f' where
  f' = rotate f r []

instance Queue RealTimeQueue where
  empty  = RTQ [] [] []
  isEmpty (RTQ [] _ _) = True
  isEmpty _            = False
  enqueue (RTQ f     r s) x = exec f (x:r) s
  head    (RTQ (x:_) _ _)   = x
  tail    (RTQ (_:f) r s)   = exec f r s
