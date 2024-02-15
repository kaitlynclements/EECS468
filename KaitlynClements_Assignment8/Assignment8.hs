{-
Author: Kaitlyn Clements
KUID: 3072622
Date: 11/24/2023
Inputs: Arithmetic Expressions
Outputs: Result or Error
Description: Haskell Program for evaluating expressions using PEMDAS
File: EECS 468 Assignment 8- PEMDAS Evaluation
-}

-- Collaborated with: Aaditi Chinawalker

-- Importing necessary libraries
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

-- Defining data types (Expr) for representative arithmetic expression (ex: addition, subtraction, etc. )
-- Includes constructors for all expressions
data Expr
    = Add Expr Expr -- Addition
    | Sub Expr Expr -- Subtraction
    | Mul Expr Expr -- Multiplication
    | Div Expr Expr -- Division
    | Mod Expr Expr -- Modular
    | Pow Expr Expr -- Power (Exponentiation)
    | Neg Expr -- Negation
    | Const Double -- Constant
    deriving (Show)

-- Define the parser for numeric constants
-- Parses numeric constants
number :: Parser Expr
number = do
    n <- read <$> many1 digit
    m <- optionMaybe (try (symbol "*" *> factor))
  -- If a * is encountered after a number, it parses the following expression
    case m of
        Just m' -> return $ Mul (Const n) m'
        Nothing -> return $ Const n

-- Define the parser for arithmetic expressions
-- Uses buildExpressionParser to parse arithmetic expressions by looking at the table of operators
expr :: Parser Expr
expr = buildExpressionParser table factor

-- Define operator precedence and associativity
-- Creating a table to define operators with their associating symbols and how they should be priotitized (Incorporating PEMDAS)
table :: [[Operator String () Identity Expr]]
table =
    [ [prefix "-" Neg],
    [binary "**" Pow AssocRight],
    [binary "*" Mul AssocLeft, binary "/" Div AssocLeft, binary "%" Mod AssocLeft],
    [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
    ]

-- Helper function to create binary operators
-- Creates binary operator using specified name, function, and associativity 
binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> Operator String () Identity Expr
binary name f assoc = Infix (f <$ symbol name) assoc

-- Helper function to create unary operators
-- Creates a prefix unary operator using the specified name and function
prefix :: String -> (Expr -> Expr) -> Operator String () Identity Expr
prefix name f = Prefix (f <$ symbol name)

-- Helper function to parse a symbol
-- Parses a specified string as a symbol, taking care of spaces around it
symbol :: String -> Parser String
symbol s = spaces *> string s <* spaces

-- Define the factor parser for handling parentheses
-- Parses factors (an expression in parenthesis or a constant)
factor :: Parser Expr
factor = parens expr <|> number

-- Helper function to parse expressions enclosed in parentheses
-- Parses expression within parenthesis using between
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Main function to parse and evaluate expressions
evaluate :: String -> Either ParseError Double --takes a string input, parses it to an expression
evaluate input = do -- Evaluates the expression using eval function
    ast <- parse expr "" input
    return $ eval ast

-- Evaluate an expression
eval :: Expr -> Double
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Mod e1 e2) = mod' (eval e1) (eval e2)
eval (Pow e1 e2) = eval e1 ** eval e2
eval (Neg e) = -eval e
-- Recursively evaluates an expression based on the structure of it
eval (Const c) = c

-- Handle division by zero
-- When dividing by zero, we are handling the error by using floor division to give a result. This means the division of zero will be Result: Infinity
mod' :: Double -> Double -> Double
mod' x y = x - realToFrac (floor (x / y)) * y

-- Main function to handle user interaction
main :: IO ()
main = do
    putStrLn "Enter an arithmetic expression:"
    input <- getLine
    case evaluate input of
        Left err -> putStrLn $ "Error: " ++ show err
        Right result -> putStrLn $ "Result: " ++ show result