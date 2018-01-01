{-|
Module      : Lib
Description : Terrible parser for a simple Lambda-Calculus based language
Copyright   : (c) Andreas Klebinger, 2017
License     : GPL-3
Stability   : experimental
Portability :

-}

module Parser(parseCode, Expression(..), Var) where

import Text.ParserCombinators.ReadP as ReadP
import Data.Char
import Control.Monad
import Data.Maybe
import Debug.Trace




munchp1 :: ReadP a -> ReadP b -> ReadP [a]
munchp1 p sep = do
  l <- p
  sep
  r <- rightSide
  return $ l:r
  where
    rightSide = munchp1 p sep <++ return []

munchp :: ReadP a -> ReadP b -> ReadP [a]
munchp p sep = munchp1 p sep<++ return []

braced :: ReadP a -> ReadP a
braced p = char '(' >> skipSpaces >> p <* char ')' <* skipSpaces

--satisfy extended to all values
satisfyp :: (a -> Bool) -> ReadP a -> ReadP a
satisfyp cond p = do
  e <- p
  if cond e then return e else mzero

type Var = String
type Vars = [String]

data Expression =
  Value { val :: Int } |
  Lambda { lvars :: Maybe Var, lbody :: Expression} |
  App {func :: Expression, arg :: Maybe Expression} |
  Variable { name :: Var } |
  Let Var Expression Expression |
  LetRec Var Expression Expression |
  Builtin String [Expression] |
  Op Expression String Expression |
  Bottom
  deriving (Eq,  Ord, Show)

reserved :: [String]
reserved = ["let", "in", "add", "if", "lambda"]

builtins :: [String]
builtins = ["add", "if"]

operators :: [String]
operators = ["+", "-", "/", "*", "==", "/="]

opsymbol :: String
opsymbol = ['+', '-', '/', '*', '=']

--Find a way to put app at the start of the list
expression :: ReadP.ReadP Expression
expression = opexpr <++ letrecp <++ papp <++ ((skipSpaces >> braced expression <* skipSpaces) <++ letp <++ lambda <++ builtin <++ opexpr <++ variable <++ value) <* skipSpaces

opexpr :: ReadP.ReadP Expression
opexpr = pure Op <*> sexpr <* skipSpaces <*> oplit <*> expression

oplit :: ReadP.ReadP String
oplit = satisfyp (`elem` operators) (munch1 (`elem` opsymbol)) <*
    skipSpaces


sexpr :: ReadP.ReadP Expression
sexpr = braced expression <++ lambda <++ letp <++ builtin <++ variable <++ value

numberValue :: ReadP.ReadP Int
numberValue =
  (read <$> munch1 isNumber) <* ReadP.skipSpaces

variableName :: ReadP.ReadP Var
variableName = do
  s <- satisfy isAlpha
  ss <- munch isAlphaNum
  when ((s : ss) `elem` reserved) mzero
  skipSpaces
  return (s:ss)

text :: ReadP.ReadP String
text = munch1 (\x -> x /= ' ' && x /= ')' && (x `notElem` map head operators)) <* skipSpaces

lambda :: ReadP.ReadP Expression
lambda = do
  choice
    [ char '\\',
      string "lambda" >> optional (char ':') >> return ' '
    ] >> skipSpaces
  vars <- munchp variableName (return ())
  choice [void (string "->"), void $ char '.'] >> skipSpaces
  -- For each variable create a nested lambda expression
  body <- expression
  let core = Lambda (listToMaybe vars) body
  if length vars < 2 then return core else
    return $ foldr (\v nestedLambda -> Lambda (Just v) nestedLambda) (Lambda (Just . last $ vars) body) (init vars)


papp :: ReadP.ReadP Expression
papp = do
  f <- sexpr
  args <- munchp sexpr $ return ()
  return $ foldl (\f a -> App f $ Just a) (App f Nothing) args
  --return $ App f arg

variable :: ReadP.ReadP Expression
variable = Variable <$> variableName

value :: ReadP.ReadP Expression
value = Value <$> numberValue

letpc :: Expression -> ReadP.ReadP Expression
letpc constructor = do
  let (keyw, con) =
        case constructor of
          Let {} -> ("let", Let)
          LetRec {} -> ("letrec", LetRec)
  string keyw >> skipSpaces
  var <- variableName
  char '=' >> skipSpaces
  def <- expression
  string "in" >> skipSpaces
  body <- expression
  return $ con var def body

letp :: ReadP.ReadP Expression
letp = letpc Let {}

letrecp :: ReadP.ReadP Expression
letrecp = letpc LetRec {}

builtin :: ReadP.ReadP Expression
builtin = do
  x <- munch1 isAlpha <* skipSpaces
  if x `elem` builtins then Builtin x <$> munchp sexpr (return ()) else fail "No builtin function"

parseCode :: String -> Expression
parseCode code =
  let result = head $ readP_to_S expression code
  in
  (if (not . null . snd) result then traceShow ("Failed to parse whole string, remaining:" ++ snd result) else id) $ fst result
