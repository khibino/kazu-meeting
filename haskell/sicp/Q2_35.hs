module Q2_35 (module Q2_35) where

data SCList a = Nil | Atom a | Cons (SCList a) (SCList a)

fromList :: [a] -> SCList a
fromList []   = Nil
fromList (x:xs) = Atom x `Cons` fromList xs


-- オリジナル
justCountLeaves :: (Integral int) => SCList int -> int
justCountLeaves = f
  where f Nil = 0
        f (Atom _) = 1
        f (Cons car cdr) = (f car) + (f cdr)


accumulate :: (t -> t -> t) -> t -> t -> SCList t1 -> t
accumulate op zero one = f
  where f Nil = zero
        f (Atom _) = one
        f (Cons l r) = op (f l) (f r)

accumulateList :: (a -> b -> b) -> b -> [a] -> b
accumulateList = foldr

--countLeavesWithMap t =
--  accumulate (+) 0 1 

countLeaves :: (Integral int) => SCList int -> int
countLeaves = accumulate (+) 0 1

testData :: (Integral int) => SCList int
testData =
  fromList [1, 2] `Cons` fromList [3, 4]

test :: IO [()]
test =
  mapM print [justCountLeaves testData,
              countLeaves testData]
