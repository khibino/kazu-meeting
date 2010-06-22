module Huffman (module Huffman) where

import qualified Data.Ord as O
import qualified Data.List as L
import Data.Sequence ((<|), (|>), (><), ViewL (EmptyL, (:<)), Seq)
import Data.Set (Set)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

data Tree =
  Leaf  (String, Int) |
  Br Tree Tree (Set String, Int)
  deriving Show

weight :: Tree -> Int
weight = f
  where f (Leaf (_, w)) = w
        f (Br _ _ (_, w)) = w
        
symbols :: Tree -> Set String
symbols  = f           
  where f (Leaf (s, _)) = Set.fromList [s]
        f (Br _ _ (ss, _)) = ss
        
merge :: Tree -> Tree -> Tree
merge x y = Br x y (Set.fromList (Set.toList (symbols x) ++
                                     Set.toList (symbols y)),
                       weight x + weight y)

instance Eq Tree where
  a == b = weight a == weight b

instance Ord Tree where
  compare = O.comparing weight

type Token = (Int, [String])

mergeT :: Token -> Token -> Token
mergeT x y = (fst x + fst y, snd x ++ snd y)

type Compose a = Seq a

empty :: Compose a
empty  = Seq.empty

insert :: Ord a => Compose a -> a -> Compose a
insert cseq akey = f (Seq.length cseq) cseq
  where f 0 cseq' = akey <| cseq'
        f n cseq' = case compare akey ckey of
          EQ -> lseq >< akey <| rest
          LT -> f lsize lseq >< rest
          GT -> (lseq |> ckey) >< f (n - lsize - 1) 
                rseq
          where lsize = quot (n - 1) 2
                (lseq, rest) = Seq.splitAt lsize cseq'
                ckey :< rseq   = Seq.viewl rest

insertList :: Ord a => Compose a -> [a] -> Compose a
insertList = L.foldl' insert

ofList :: Ord a => [a] -> Compose a
ofList  = insertList empty

composeStep :: Ord a => (a -> a -> a) -> Compose a -> Maybe (Compose a)
composeStep mergeF = step . Seq.viewl
  where step EmptyL      = undefined
        step (l0 :< xs0) = f1 (Seq.viewl xs0)
          where f1 EmptyL       = Nothing
                f1 (l1 :< rest) = Just (insert rest (mergeF l0 l1))

makeEncoder :: (Ord a) => (a -> a -> a) -> Compose a -> a
makeEncoder mergeF cseq = let (rv :< _) = Seq.viewl $ go cseq in rv
  where go cseq' = maybe cseq' go $ composeStep mergeF cseq'

makeTreeEncoder :: Compose Tree -> Tree
makeTreeEncoder  = makeEncoder merge

-- 問題 2.69
makeEncoderFromList :: [(String, Int)] -> Tree
makeEncoderFromList =  makeTreeEncoder . ofList . map Leaf

encodeToken :: Compose Token -> Token
encodeToken = makeEncoder mergeT

d0 :: [(String, Int)]
d0 = [("A", 8), ("B", 3), ("C", 1), ("D", 1),
      ("E", 1), ("F", 1), ("G", 1), ("H", 1)]

d0token :: Compose Token
d0token = ofList $ map (uncurry $ flip (curry id) . (: [])) d0

d0compose :: Compose Tree
d0compose =  ofList $ map Leaf d0

d0tree :: Tree
d0tree =  makeTreeEncoder d0compose

decode :: String -> Tree -> [String]
decode bits tree =
  decode_1 bits tree
  where decode_1 ""     _ = []
        decode_1 (b:bs) curNode =
          case choose b curNode of
            Leaf (s, _) -> s : decode_1 bs tree
            nextN       -> decode_1 bs nextN
        choose b (Br l r _) | b == '0'  = l
                               | b == '1'  = r
                               | otherwise = undefined
        choose _ (Leaf _)      = undefined

showTreeCodes      :: Tree -> String
showTreeCodes tree =  rec tree ""
  where rec (Leaf  (chr, _)) bits = chr ++ ":" ++ reverse bits ++ "\n"
        rec (Br l r _) bits = rec l ('0' : bits) ++ rec r ('1' : bits)

encodeSymbol :: String -> Tree -> String
encodeSymbol sym = check . (:[]) . tryE ""
  where err = error ("Unknown symbol: " ++ sym)
        tryE bits (Leaf (x, _)) | sym == x  = Just bits
                                 | otherwise = Nothing
        tryE bits (Br l r (symset, _))
          | Set.member sym symset =
            Just (check [tryE (bits ++ "0") l, tryE (bits ++ "1") r])
          | otherwise  = Nothing
        check = fromMaybe err . foldr1 (\ e r -> if isJust e then e else r)

encodeSymbolOther :: String -> Tree -> String
encodeSymbolOther sym = f []
  where err = error ("Unknown symbol: " ++ sym)
        contains (Leaf (x, _))        = x == sym
        contains (Br _ _ (symset, _)) = Set.member sym symset
        f bits l@(Leaf _) | contains l = bits
                          | otherwise  = err
        f bits (Br l r _) | contains l = f (bits ++ "0") l
                             | contains r = f (bits ++ "1") r
                             | otherwise  = err

-- 問題 2.68
encode :: [String] -> Tree -> String
encode msg tree =  f msg
  where f = foldr ((++) . (`encodeSymbol` tree)) ""

--        f = foldr (\ x -> (++) (encodeSymbol x tree)) ""
--        f [] = ""
--        f (x:xs) = encodeSymbol x tree ++ f xs

-- 問題 2.67
symbolQ2q67 :: [(String, Int)]
symbolQ2q67 =  [("A", 4), ("B", 2), ("D", 1), ("C", 1)]

treeQ2q67 :: Tree
treeQ2q67 =  makeEncoderFromList symbolQ2q67

messageQ2q67 :: String 
messageQ2q67 =  "0110010101110"

decodeQ2q67 :: [String]
decodeQ2q67 =  decode messageQ2q67 treeQ2q67

encodeQ2q67 :: String
encodeQ2q67 =  encode decodeQ2q67 treeQ2q67


--問題 2.70
symbolQ2q70 :: [(String, Int)]
symbolQ2q70 =  [("a",    2), ("na", 16),
                      ("boom", 1), ("Sha", 3),
                      ("Get",  2), ("yip", 9),
                      ("job",  2), ("Wah", 1)]

treeQ2q70 :: Tree
treeQ2q70 =  makeEncoderFromList symbolQ2q70

stmtQ2q70 :: [String]
stmtQ2q70 =  words $ unlines ["Get a job",
                                    "Sha na na na na na na na na",
                                    "Get a job",
                                    "Sha na na na na na na na na",
                                    "Wah yip yip yip yip yip yip yip yip yip",
                                    "Sha boom"]

encodeQ2q70 :: String
encodeQ2q70 =  encode stmtQ2q70 treeQ2q70
