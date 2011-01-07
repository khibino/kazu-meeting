module Q3_05 where

import System.Random
import Data.List

monteCarlo                  :: (Integral a, Enum a, Fractional b) => a -> Bool -> b
monteCarlo trials experiment = passed / try
  where (passed, try) = foldr (\ ex (passed', try') ->
                                ((if ex then 1 else 0) + passed', try' + 1))
                        (0, 0) ([1..trials] >> [experiment])

monteCarloL     :: (Fractional b) => [Bool] -> b
monteCarloL expL = passed / try
  where (passed, try) = foldl' (\ (passed', try') ex ->
                                ((if ex then 1 else 0) + passed', try' + 1))
                        (0, 0) expL

type PredType a = a -> a -> Bool

predPi    :: PredType Double
predPi x y = x^2 + y^2 <= 1.0

adjustRange                   :: StdGen -> (Double, Double) -> Double -> Double
adjustRange gen (min', max') n =
  let (gmin, gmax) = genRange gen in
  n * ((max' - min') / fromIntegral (gmax - gmin)) + min'

testR0 :: IO Double
testR0 = fmap (\ gen -> adjustRange gen ((-1), 1) (-1000000000)) newStdGen

adjustList                 :: StdGen -> (Double, Double) -> [Double] -> [Double]
adjustList gen (min', max') = map (adjustRange gen (min', max'))

intRandoms :: StdGen -> [Int]
intRandoms gen = nv : intRandoms ngen
  where (nv, ngen) = next gen

testL0 :: IO [Double]
testL0  = fmap (\ gen -> adjustList gen ((-1), 1) $ take 20 (map fromIntegral (intRandoms gen))) newStdGen

divList             :: [a] -> ([a], [a])
divList (x1':x2':xs) =
          let (xs1, xs2) = divList xs in (x1' : xs1, x2' : xs2)
divList [x]      = ([x], [])
divList []       = ([], [])

getRList           :: Int -> StdGen -> ([Double], [Double])
getRList trials gen = divList $ map (fromIntegral :: Int -> Double) $ take (trials * 2) (intRandoms gen)

testD0 :: IO ([Double], [Double])
testD0 =  fmap (getRList 20) newStdGen

estimateIntegral :: (Fractional b) =>
                    StdGen -> PredType Double
                    -> Double -> Double -> Double -> Double -> Int -> b
estimateIntegral gen pre x1 y1 x2 y2 trials = monteCarloL results
  where (l1, l2) = getRList trials gen
        
        results = zipWith pre
                  (adjustList gen (x1, x2) l1)
                  (adjustList gen (y1, y2) l2)

test0 :: Fractional b => IO b
test0 =  fmap (\ gen -> estimateIntegral gen predPi (-1) (-1) 1 1 50000) newStdGen
