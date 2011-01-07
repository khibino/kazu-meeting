module Sec3_3 where

--import Data.List hiding (head, tail)
import qualified Data.List as L

--Q 3.13
--  lazy にコンシングできるので書き換えなくても cyclic にできる

cycle' :: [a] -> [a]
cycle' l = l ++ cycle' l


--Q 3.14
--  revserse のコンシングは必要だが、コンシングはlazyに行なわれる

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


-- そもそも双方向キューにしておけば双方から何度使っても
-- コストが平均化される

data DeQue a = DeQue { head' :: [a]
                     , tail' :: [a] }
                      
empty :: DeQue a
empty =  DeQue [] []

null :: DeQue a -> Bool
null q =  L.null (head' q) && L.null (tail' q)

cons :: a -> DeQue a -> DeQue a
cons x xs = xs { head' = x : head' xs }

snoc :: DeQue a -> a -> DeQue a
snoc xs x = xs { tail' = x : tail' xs }

head  :: DeQue a -> a
head (DeQue h t) =  L.head $ h ++ L.reverse t

last  :: DeQue a -> a
last (DeQue h t) = L.head $ t ++ L.reverse h

--tail :: DeQue a -> DeQue a
--tail (DeQue h t) =  L.tail $ h ++ L.reverse t

--init :: DeQue a -> DeQue a
--init (DeQue h t) =  L.tail $ t ++ L.reverse h

--Q 3.15


--Q 3.16
--  cellが共有された構造を意識していない


--Q 3.17
--  Haskellではオブジェクト等価比較ができない


--Q 3.18
