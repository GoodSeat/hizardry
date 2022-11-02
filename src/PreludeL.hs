module PreludeL (module Prelude, (!!))
where

import GHC.Stack (HasCallStack)
import Prelude hiding ((!!))

-- | List index (subscript) operator, starting from 0.
-- It is an instance of the more general 'Data.List.genericIndex',
-- which takes an index of any integral type.
--
-- >>> ['a', 'b', 'c'] !! 0
-- 'a'
-- >>> ['a', 'b', 'c'] !! 2
-- 'c'
-- >>> ['a', 'b', 'c'] !! 3
-- *** Exception: Prelude.!!: index too large
-- >>> ['a', 'b', 'c'] !! (-1)
-- *** Exception: Prelude.!!: negative index
--
(!!) :: HasCallStack => [a] -> Int -> a
(e:es) !! 0 = e
(e:es) !! n = if n < 0 then error "negative index" else es !! (n - 1)
[]     !! n = error "index too large"

