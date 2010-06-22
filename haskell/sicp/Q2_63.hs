module Q2_63 (module Q2_63) where

import BinTree

fig2x16x0 = Trunk
             (Trunk
              (Trunk Leaf 1 Leaf)
              3
              (Trunk Leaf 5 Leaf))
             7
             (Trunk
              Leaf
              9
              (Trunk Leaf 11 Leaf))
             
fig2x16x1 = Trunk
            (Trunk Leaf 1 Leaf)
            3
            (Trunk
             (Trunk Leaf 5 Leaf)
             7
             (Trunk
              Leaf
              9
              (Trunk Leaf 11 Leaf)))


fig2x16x2 = Trunk
            (Trunk
             (Trunk Leaf 1 Leaf)
             3
             Leaf)
            5
            (Trunk
             (Trunk Leaf 7 Leaf)
             9
             (Trunk Leaf 11 Leaf))

fig2x16x0, fig2x16x1, fig2x16x2 :: Tree Int


tests :: [(String, [Int])]
tests  = [ (fn ++ " " ++ dn, f d) |
           (fn, f) <- [("f1", treeToList1), ("f2", treeToList2)],
           (dn, d) <- [("d0", fig2x16x0), ("d1", fig2x16x1), ("d2", fig2x16x2)] ]


{-

b.

木の深さを m ノード数を n とすると、
n = 2^m -1

tree->list-1 のコストを f(m)、
tree->list-2 のコストを g(m)、
とする。

g(m) = 2 * g(m - 1) + 1
                    ~~~consのコスト
g(m) + 1 = 2 * ( g(m - 1) + 1 ) = ... = 2^m * (g(0) + 1) = 2^m = n + 1 = O(n)


      f(m) = 2 * f(m - 1) + 2^(m-1)
                            ~~~~~~~appendとconsのコスト

しかしこれは strict な評価戦略の場合である。
lazy の場合には append のコストは


2 * f(m-1) = 2^2 * f(m - 2) + 2^(m-1)

...

2^(m-1) * f(1) = 2^m * f(0) + 2^(m-1)


       f(m) = 2^m * f(0) + m * 2^(m-1)
            =        (log n) * (n + 1) / 2 = O(n log n)
-}
