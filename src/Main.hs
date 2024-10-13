{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import System.Exit

data Operator
    = Plus
    | Minus
    | Product
    | Division
    | Exponent
    deriving(Show)

data Associativity = L | R
    deriving(Eq)

precedence :: Operator -> Int
precedence Plus = 1
precedence Minus = 1
precedence Product = 2
precedence Division = 2
precedence Exponent = 3

associativity :: Operator -> Associativity
associativity Exponent = R
associativity _ = L

data Parenthesis
    = Open
    | Closed
    deriving(Show)

data Token
    = Number Float
    | Operator Operator
    | Parenthesis Parenthesis
    deriving(Show)

type InfixExpr = [Token]
type PostfixExpr = [Token]

evalPostfixExpr :: PostfixExpr -> Float
evalPostfixExpr tokens =
    evalPostfixExprRec tokens []
    where
        evalPostfixExprRec (Number num:tl) stack = evalPostfixExprRec tl (Number num:stack)
        evalPostfixExprRec (Operator op:tl) (Number x2:Number x1:stack) =
            let result = Number (case op of
                            Plus -> x1 + x2
                            Minus -> x1 - x2
                            Product -> x1 * x2
                            Division -> x1 / x2
                            Exponent -> x1 ** x2)
            in evalPostfixExprRec tl (result:stack)
        evalPostfixExprRec [] stack = case head stack of
                                            Number num -> num
                                            _ -> undefined
        evalPostfixExprRec (Parenthesis _:_) _ = undefined

infixToPostfixExpr :: InfixExpr -> PostfixExpr
infixToPostfixExpr tokens =
    infixToPostfixExprRec tokens []
    where
        infixToPostfixExprRec :: [Token] -> [Token] -> [Token]
        infixToPostfixExprRec (Number num:tl) stack = Number num:infixToPostfixExprRec tl stack
        infixToPostfixExprRec (Parenthesis Open:tl) stack = infixToPostfixExprRec tl (Parenthesis Open:stack)
        infixToPostfixExprRec (Parenthesis Closed:tl) (stackHd:stack) = case stackHd of
            Parenthesis Open -> infixToPostfixExprRec tl stack
            _ -> stackHd:infixToPostfixExprRec (Parenthesis Closed:tl) stack
        infixToPostfixExprRec (Operator op:tl) (Operator stackOp:stack) =
            if precedence op < precedence stackOp ||
                associativity op == L && precedence op == precedence stackOp
            then Operator stackOp:infixToPostfixExprRec (Operator op:tl) stack
            else infixToPostfixExprRec tl (Operator op:Operator stackOp:stack)
        infixToPostfixExprRec (Operator op:tl) stack = infixToPostfixExprRec tl (Operator op:stack)
        infixToPostfixExprRec [] stack = stack

digitCount :: Integer -> Int
digitCount = go 1 . abs
    where
        go _ 0 = 0
        go ds n = if n >= 10
            then go (ds + 1) (n `div` 10)
            else ds

tokenize :: String -> InfixExpr
tokenize (' ':str) = tokenize str
tokenize ('(':str) = Parenthesis Open:tokenize str
tokenize (')':str) = Parenthesis Closed:tokenize str
tokenize ('+':str) = Operator Plus:tokenize str
tokenize ('-':str) = Operator Minus:tokenize str
tokenize ('*':str) = Operator Product:tokenize str
tokenize ('/':str) = Operator Division:tokenize str
tokenize ('^':str) = Operator Exponent:tokenize str
tokenize ('.':str) =
    let (Number num:tl) = tokenize str
    in Number (num / fromIntegral 10^digitCount (round num)):tl
tokenize ('0':str) = undefined
tokenize ('1':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (1 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 1:tokens
tokenize ('2':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (2 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 2:tokens
tokenize ('3':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (3 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 3:tokens
tokenize ('4':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (4 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 4:tokens
tokenize ('5':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (5 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 5:tokens
tokenize ('6':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (6 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 6:tokens
tokenize ('7':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (7 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 7:tokens
tokenize ('8':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (8 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 8:tokens
tokenize ('9':str) =
    case tokenize str of
        (Number num:tl) -> let num' = fromIntegral (9 * 10^digitCount (round num)) + num
                           in Number num':tl
        tokens -> Number 9:tokens
tokenize (ch:str) = undefined
tokenize "" = []

eval :: String -> Float
eval = evalPostfixExpr . infixToPostfixExpr . tokenize

evalAboba :: InfixExpr -> Float
evalAboba = evalPostfixExpr . infixToPostfixExpr

main :: IO ()
main = do
    print "Enter expression or 'q' to exit"
    input <- getLine
    if input == "q" || input == ":q" then do
        print "See ya"
        exitSuccess
    else do
        print $ show $ eval input
        main
