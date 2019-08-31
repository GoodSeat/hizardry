module Formula (
      Formula
    , Formula.parse
    , parse'
    , eval
) where

import System.Random
import Text.ParserCombinators.Parsec hiding (token)
import qualified Data.Map as Map
import Control.Applicative ((<*>), (<*), (*>))
import qualified Control.Monad.State as State
import Control.Monad.Except

main = do
    print $ Formula.parse "(5+2)*2+3d2"

parse :: String -> Either ParseError Formula
parse s = Text.ParserCombinators.Parsec.parse expr "" s

parse' :: String -> Formula
parse' s = case Formula.parse s of Right f -> f
                                   Left  _ -> undefined

-- ================================================================================
--
{- 
 - expr    ::= term + expr | term - expr | term
 - term    ::= factor * term | factor / term | factor
 - factor  ::= (expr) | value
 - value   ::= integer | dice | var
 - integer ::= ... -2 | -1 | 0 | 1 | 2 ...
 - dice    ::= natural 'd' natural
 - natural ::= 0 | 1 | 2 ...
 -}

-- | type of operator.
data Operator = Addition | Subtraction | Production | Division

instance Show Operator where
    show Addition    = "+"
    show Subtraction = "-"
    show Production  = "*"
    show Division    = "/"

-- | formula expression.
data Formula = Value Int
             | Operate Operator Formula Formula
             | Variable String
             | Dice Int Int

instance Show Formula where
    show (Value n)       = show n
    show (Operate o n m) = "(" ++ show n ++ show o ++ show m ++ ")"
    show (Variable v)    = show v
    show (Dice n m)      = show n ++ "d" ++ show m

expr :: GenParser Char st Formula
expr =  try (Operate Addition    <$> term <*> (char '+' >> expr))
    <|> try (Operate Subtraction <$> term <*> (char '-' >> expr))
    <|> term

term :: GenParser Char st Formula
term =  try (Operate Production <$> factor <*> (char '*' >> term))
    <|> try (Operate Division   <$> factor <*> (char '/' >> term))
    <|> factor

factor :: GenParser Char st Formula
factor =  (char '(' *> expr <* char ')')
      <|> try dice
      <|> try (Value <$> integer)
      <|> variable

token :: GenParser Char st a -> GenParser Char st a
token p = many (char ' ') *> p <* many (char ' ')

natural :: GenParser Char st Int
natural = read <$> many1 digit

integer :: GenParser Char st Int
integer = (char '-' >> (*(-1)) <$> natural) <|> natural

variable :: GenParser Char st Formula
variable = Variable <$> (many1 $ noneOf " +-*/")

dice :: GenParser Char st Formula
dice = Dice <$> natural <*> (char 'd' >> natural)

-- ================================================================================

eval :: Map.Map String Int -> Formula -> StdGen -> (Either String Int, StdGen)
eval m f g = State.runState (runExceptT $ eval' m f) g

eval' :: Map.Map String Int -> Formula -> ExceptT String (State.State StdGen) Int
eval' _ (Value n) = return n
eval' m (Operate Addition    n1 n2) = (+) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Subtraction n1 n2) = (-) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Production  n1 n2) = (*) <$> eval' m n1 <*> eval' m n2
eval' m (Operate Division    n1 n2) = div <$> eval' m n1 <*> eval' m n2
eval' m (Variable name) = case Map.lookup name m of Nothing -> throwError $ "not defined value of " ++ name
                                                    Just v  -> return v
eval' _ (Dice n1 n2) = do
    (v, g') <- randomR (1, n2) <$> State.get
    State.put g'
    return $ n1 * v
