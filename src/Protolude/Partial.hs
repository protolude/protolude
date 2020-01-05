module Protolude.Partial (
  head,
  init,
  tail,
  last,
  foldl,
  foldr,
  foldl',
  foldr',
  foldr1,
  foldl1,
  cycle,
  maximum,
  minimum,
  (!!),
  sum,
  product,
  fromJust,
  read,
) where

import Data.List (head, init, tail, last, maximum, minimum, (!!), cycle)
import Data.Foldable (sum, product, foldl, foldr, foldl', foldr', foldl1, foldr1)
import Data.Maybe (fromJust)
import Text.Read (read)
