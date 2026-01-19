module PreludeL (module Prelude, (!!), read)
where

import GHC.Stack (HasCallStack)
import Prelude hiding ((!!), read)
import Text.Read (readMaybe)

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

-- | The 'read' function reads input from a string, which must be
-- completely consumed by the input process. 'read' fails with an 'error' if the
-- parse is unsuccessful, and it is therefore discouraged from being used in
-- real applications. Use 'readMaybe' or 'readEither' for safe alternatives.
--
-- >>> read "123" :: Int
-- 123
--
-- >>> read "hello" :: Int
-- *** Exception: Prelude.read: no parse
read :: HasCallStack => Read a => String -> a
read s = case readMaybe s of Just r  -> r
                             Nothing -> error "no parse"
