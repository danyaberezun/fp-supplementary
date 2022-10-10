module BankersQueue (BankersQueue) where
import Prelude hiding (head, tail)
import Queue

--- everything except front (first [a]) can be replaced with strict data
data BankersQueue a = BQ [a] Int [a] Int

inv f lf r lr
  | lr <= lf  = BQ f lf r lr
  | otherwise = BQ (f ++ reverse r) (lf + lr) [] 0

instance Queue BankersQueue where
  empty  = BQ [] 0 [] 0
  isEmpty (BQ _     lf _ _ )   = lf == 0
  enqueue (BQ f     lf r lr) x = inv f lf (x:r) (lr + 1)
  head    (BQ (x:_) _  _ _ )   = x
  tail    (BQ (_:f) lf r lr)   = inv f (lf - 1) r lr
