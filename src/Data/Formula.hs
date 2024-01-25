module Data.Formula (
      Formula
    , Data.Formula.parse
    , parse'
    , evalFormula
) where

import System.Random
import Text.ParserCombinators.Parsec hiding (token)
import qualified Data.Map as Map
import Control.Applicative ((<*>), (<*), (*>))
import qualified Control.Monad.State as State
import Control.Monad.Except

main = do
    print $ Data.Formula.parse "(5+2)*2+3d2"

parse :: String -> Either ParseError Formula
parse = Text.ParserCombinators.Parsec.parse expr ""

parse' :: String -> Formula
parse' s = case Data.Formula.parse s of Right f -> f
                                        Left  _ -> undefined

-- ================================================================================
--
{- 
 - cmpr    ::= expr = cmpr | expr < cmpr | expr <= cmpr | expr > cmpr | expr >= cpmr | expr
 - expr    ::= term + expr | term - expr | term
 - term    ::= factor * term | factor / term | factor
 - factor  ::= (expr) | value
 - value   ::= integer | dice | var
 - integer ::= ... -2 | -1 | 0 | 1 | 2 ...
 - dice    ::= natural 'd' natural
 - natural ::= 0 | 1 | 2 ...
 -}

-- | type of operator.
data Operator = Equal
              | LesserThan
              | LesserOrEqual
              | GreaterThan
              | GreaterOrEqual
              | Addition
              | Subtraction
              | Production
              | Division
              | Surplus
  deriving Eq

instance Show Operator where
    show Equal          = "="
    show LesserThan     = "<"
    show LesserOrEqual  = "<="
    show GreaterThan    = ">"
    show GreaterOrEqual = ">="
    show Addition       = "+"
    show Subtraction    = "-"
    show Production     = "*"
    show Division       = "/"
    show Surplus        = "%"

-- | formula expression.
data Formula = Value Int
             | Operate Operator Formula Formula
             | Variable String
             | Dice Int Int
             | MinOf Formula Formula
             | MaxOf Formula Formula
  deriving Eq

instance Show Formula where
    show (Value n)       = show n
    show (Operate o n m) = "(" ++ show n ++ show o ++ show m ++ ")"
    show (Variable v)    = show v
    show (Dice n m)      = show n ++ "d" ++ show m
    show (MinOf n m)     = "min(" ++ show n ++ "," ++ show m ++ ")"
    show (MaxOf n m)     = "max(" ++ show n ++ "," ++ show m ++ ")"

cmpr :: GenParser Char st Formula
cmpr =  try (Operate Equal          <$> token expr <*> (char '=' >> token cmpr))
    <|> try (Operate LesserThan     <$> token expr <*> (char '<' >> token cmpr))
    <|> try (Operate LesserOrEqual  <$> token expr <*> (string "<=" >> token cmpr))
    <|> try (Operate GreaterThan    <$> token expr <*> (char '>' >> token cmpr))
    <|> try (Operate GreaterOrEqual <$> token expr <*> (string ">=" >> token cmpr))
    <|> token expr

expr :: GenParser Char st Formula
expr =  try (Operate Addition    <$> token term <*> (char '+' >> token expr))
    <|> try (Operate Subtraction <$> token term <*> (char '-' >> token expr))
    <|> token term

term :: GenParser Char st Formula
term =  try (Operate Production <$> token factor <*> (char '*' >> token term))
    <|> try (Operate Division   <$> token factor <*> (char '/' >> token term))
    <|> try (Operate Surplus    <$> token factor <*> (char '%' >> token term))
    <|> token factor

factor :: GenParser Char st Formula
factor =  (char '(' *> token expr <* char ')')
      <|> try dice
      <|> try (Value <$> integer)
      <|> try minOf
      <|> try maxOf
      <|> variable

token :: GenParser Char st a -> GenParser Char st a
token p = many (char ' ') *> p <* many (char ' ')

natural :: GenParser Char st Int
natural = read <$> many1 digit

integer :: GenParser Char st Int
integer = (char '-' >> (*(-1)) <$> natural) <|> natural

variable :: GenParser Char st Formula
variable = Variable <$> many1 (noneOf " +-*/")

dice :: GenParser Char st Formula
dice = Dice <$> natural <*> (char 'd' >> natural)

minOf :: GenParser Char st Formula
minOf = MinOf <$> (string "min(" *> expr) <*> (char ',' *> expr) <* token (char ')')

maxOf :: GenParser Char st Formula
maxOf = MaxOf <$> (string "max(" *> expr) <*> (char ',' *> expr) <* token (char ')')

-- ================================================================================

evalFormula :: Map.Map String Int -> Formula -> StdGen -> (Either String Int, StdGen)
evalFormula m f = State.runState (runExceptT $ eval' m f)

eval' :: Map.Map String Int -> Formula -> ExceptT String (State.State StdGen) Int
eval' _ (Value n) = return n
eval' m (Operate Equal          n1 n2) = fmap boolToInt $ (==) <$> eval' m n1 <*> eval' m n2
eval' m (Operate LesserThan     n1 n2) = fmap boolToInt $ (< ) <$> eval' m n1 <*> eval' m n2
eval' m (Operate LesserOrEqual  n1 n2) = fmap boolToInt $ (<=) <$> eval' m n1 <*> eval' m n2
eval' m (Operate GreaterThan    n1 n2) = fmap boolToInt $ (> ) <$> eval' m n1 <*> eval' m n2
eval' m (Operate GreaterOrEqual n1 n2) = fmap boolToInt $ (>=) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Addition       n1 n2) = (+) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Subtraction    n1 n2) = (-) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Production     n1 n2) = (*) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Division       n1 n2) = div <$> eval' m n1 <*> eval' m n2
eval' m (Operate Surplus        n1 n2) = mod <$> eval' m n1 <*> eval' m n2
eval' m (Variable name) = case Map.lookup name m of Nothing -> throwError $ "not defined value of " ++ name
                                                    Just v  -> return v
eval' _ (Dice 0   _) = return 0
eval' _ (Dice n1  1) = return n1
eval' m (Dice n1 n2) = do
    (v, g') <- State.gets (randomR (1, n2))
    State.put g'
    (v + ) <$> eval' m (Dice (n1 - 1) n2)

eval' m (MinOf n1 n2) = min <$> eval' m n1 <*> eval' m n2
eval' m (MaxOf n1 n2) = max <$> eval' m n1 <*> eval' m n2

boolToInt b = if b then 1 else 0
