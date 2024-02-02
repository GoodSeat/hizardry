{-# LANGUAGE FlexibleContexts #-}
module Data.Formula (
      Formula
    , Data.Formula.parse
    , parse'
    , evalFormula
) where

import System.Random
import Text.ParserCombinators.Parsec hiding (token)
import Text.Parsec.Expr
import qualified Data.Map as Map
import Control.Applicative ((<*>), (<*), (*>))
import qualified Control.Monad.State as State
import Control.Monad.Except
import Text.Read (Read(readsPrec))

main = do
    print $ Data.Formula.parse "(5+2)*2+3d2"
    print $ Data.Formula.parse "(1-7+50)*100/70"
    print $ Data.Formula.parse "min(1,3)"
    print $ Data.Formula.parse "min(agi*6, 95)"
    let f = read "min(agi*6, 95)" :: Formula
    print f
    let fs = read "[min(agi*6, 95),(5+2)*2+3d2]" :: [Formula]
    print fs

parse :: String -> Either ParseError Formula
parse = Text.ParserCombinators.Parsec.parse cmpr ""

parse' :: String -> Formula
parse' s = case Data.Formula.parse s of Right f -> f
                                        Left msg -> error $ show msg

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
data OperatorKind = Equal
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

instance Show OperatorKind where
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
             | Operate OperatorKind Formula Formula
             | Variable String
             | Dice Formula Formula
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

instance Read Formula where
    readsPrec n t = let t' = takeNextComma t 0;
                        f' = Data.Formula.parse t'
                    in case f' of Right f -> [(f, drop (length t') t)]
                                  _       -> []
      where
        takeNextComma (',':_)  0 = []
        takeNextComma (']':_)  _ = []
        takeNextComma ('(':cs) n = '(' : takeNextComma cs (n + 1)
        takeNextComma (')':cs) n = ')' : takeNextComma cs (n - 1)
        takeNextComma (c:cs)   n =  c  : takeNextComma cs n
        takeNextComma []       _ = []

cmpr = buildExpressionParser table factor <?> "expression"
  where
    table = [
             [binary "*" (Operate Production ) AssocLeft, binary "/"  (Operate Division      ) AssocLeft, binary "%" (Operate Surplus   ) AssocLeft]
            ,[binary "+" (Operate Addition   ) AssocLeft, binary "-"  (Operate Subtraction   ) AssocLeft]
            ,[binary "=" (Operate Equal      ) AssocLeft
             ,binary "<" (Operate LesserThan ) AssocLeft, binary "<=" (Operate LesserOrEqual ) AssocLeft
             ,binary ">" (Operate GreaterThan) AssocLeft, binary ">=" (Operate GreaterOrEqual) AssocLeft
             ]
            ]
    binary mark fun = Infix (string mark >> return fun)
   
factor :: GenParser Char st Formula
factor =  (char '(' *> token cmpr <* char ')')
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
variable = Variable <$> many1 (noneOf " +-*/%=<>()")

dice :: GenParser Char st Formula
dice =  Dice <$> (try (Value <$> natural) <|> (char '(' *> cmpr <* char ')'))
             <*> (char 'd' >> 
                 (try (Value <$> natural) <|> (char '(' *> cmpr <* char ')')))

minOf :: GenParser Char st Formula
minOf = MinOf <$> (string "min(" *> token cmpr) <*> (char ',' *> token cmpr) <* token (char ')')

maxOf :: GenParser Char st Formula
maxOf = MaxOf <$> (string "max(" *> token cmpr) <*> (char ',' *> token cmpr) <* token (char ')')

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
eval' _ (Dice (Value 0)  _) = return 0
eval' m (Dice n1 (Value 1)) = eval' m n1
eval' m (Dice n1 n2)        = do
    n1' <- eval' m n1
    n2' <- eval' m n2
    (v, g') <- State.gets (randomR (1, n2'))
    State.put g'
    (v + ) <$> eval' m (Dice (Value $ n1' - 1) n2)

eval' m (MinOf n1 n2) = min <$> eval' m n1 <*> eval' m n2
eval' m (MaxOf n1 n2) = max <$> eval' m n1 <*> eval' m n2

boolToInt b = if b then 1 else 0
