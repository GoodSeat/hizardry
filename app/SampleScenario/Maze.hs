module SampleScenario.Maze where

import PreludeL
import Data.Maze

maze1F :: Maze
maze1F = fromText (lines txt) (4, 5)
  where
    txt = "+--------+--+\n" ++
          "|     K  |[]|\n" ++
          "|  +^-+  +--+\n" ++
          "|  |_ >  |##|\n" ++
          "|  |   ^^|  |\n" ++
          "|  D     =##|\n" ++
          "|++|     |  |\n" ++
          "|  |     |  |\n" ++
          "|  +v-+vv+  |\n" ++
          "|~ <  =  |_ |\n" ++
          "+-----+-----+\n"

maze1F' :: Maze
maze1F' = fromText (lines txt) (4, 5)
  where
    txt = "+--------+--+\n" ++
          "|     K  |[]|\n" ++
          "|  +^-+  +--+\n" ++
          "|  |_ >  |##|\n" ++
          "|  |   ^^|  |\n" ++
          "|  D     =##|\n" ++
          "|++|     |  |\n" ++
          "|  |     =  |\n" ++
          "|  +v-+vv+  |\n" ++
          "|~ <  =  |_ |\n" ++
          "+-----+-----+\n"

maze2F :: Maze
maze2F = fromText (lines txt) (6, 5)
  where
    txt = "+--------+--+--+--+\n" ++
          "|     K  |[]|[]|[]|\n" ++
          "|  +^-+  +--+--+  +\n" ++
          "|  |~ >  |## ##   |\n" ++
          "|  |   ^^|        |\n" ++
          "|  D     =## ##   |\n" ++
          "|++|     |        |\n" ++
          "|  |     |        |\n" ++
          "|  +v-+vv+        |\n" ++
          "|  <  =  |_       |\n" ++
          "+-----+-----+--+--+\n"


