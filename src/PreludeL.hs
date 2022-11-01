module PreludeL (module Prelude, (!!))
where

import GHC.Stack (HasCallStack)
import Prelude hiding ((!!))

(!!) :: HasCallStack => [a] -> Int -> a
(e:es) !! 0 = e
(e:es) !! n = if n < 0 then error "index too small" else es !! (n - 1)
[]     !! n = error "index too large"

