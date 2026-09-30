||| Order-preserving deduplication and set membership in n log n.
|||
||| The path-coverage join used `nub` and `elem` over plain lists. Measured
||| 2026-09-30 on luci pkgs/Luci (24429 obligations, 135978 hit lines, 12185
||| distinct hit ids): one `buildPathCoverageResultFromHits` took 313 s and
||| `evidenceCounts` another 80 s, and idris2-cov and lazy each paid it once per
||| step4 run. These helpers return exactly what the list forms returned — the
||| first element of each key survives, in input order — so every caller keeps
||| its output byte for byte and only the cost changes.
module Coverage.Core.OrdNub

import public Data.SortedSet

%default total

||| `nub` on a key: keep the FIRST element of each key, in input order. The same
||| list `nubBy (\a, b => key a == key b)` returns, for any key whose `Ord`
||| agrees with its `Eq`.
public export
nubOrdOn : Ord k => (a -> k) -> List a -> List a
nubOrdOn key = go empty []
  where
    go : SortedSet k -> List a -> List a -> List a
    go _ acc [] = reverse acc
    go seen acc (x :: xs) =
      let kx = key x in
      if contains kx seen
         then go seen acc xs
         else go (insert kx seen) (x :: acc) xs

||| `nub`, in n log n, same result.
public export
nubOrd : Ord a => List a -> List a
nubOrd = nubOrdOn id

||| Membership test against a set built once, for `filter` over a long list.
public export
inSet : Ord a => SortedSet a -> a -> Bool
inSet s x = contains x s
