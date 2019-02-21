module Lib where

import Control.Monad
import Control.Monad.Writer

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BinOp = AddOp
           | SubOp
           | MulOp
           | DivOp
           | AndOp
           | OrOp
           | GtOp
           | LtOp
           | EqOp
           deriving Show

data Val = IntegerVal Integer
         | StringVal String
         | BoolVal Bool
         
instance Show Val where
    show a = 
        case a of
            IntegerVal i -> show i
            StringVal str -> "\"" ++ str ++ "\""
            BoolVal b -> if b == True then "%true" else "%false"

data Expr = Binary BinOp Expr Expr
          | Neg Expr
          | NotExpr Expr
          | Value Val
          | Identifier String
          | FunCallExpr String [Expr]
          | If Expr Expr Expr
          deriving Show

data Stmt = Seq [Stmt]
          | Assign String Expr
          | FunCallStmt String [Expr]
          | Return Expr
          deriving Show

data Function = Function String [String] Stmt deriving Show


languageDef =
    emptyDef { Token.commentStart = "/*"
             , Token.commentEnd = "*/"
             , Token.commentLine = "//"
             , Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.reservedNames = ["fn", "return", "true", "false", "if", "then", "else"]
             , Token.reservedOpNames = ["+", "-", "*", "/", "=", "&&", "||", "!", ">", "<"]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
stringLiteral = Token.stringLiteral lexer
semi = Token.semi lexer
comma = Token.comma lexer
whiteSpace = Token.whiteSpace lexer

rubbishParser :: Parser [Function]
rubbishParser = many1 (whiteSpace >> function)

function :: Parser Function
function = do
            reserved "fn"
            name <- identifier
            args <- parens (sepBy identifier comma)
            stmts <- braces statement
            return $ Function name args stmts

statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt

sequenceOfStmt =
    do
        list <- (endBy1 oneStatement semi)
        return $ if length list == 1 then head list else Seq list

oneStatement = choice [try returnStmt, try funCallStmt, assignStmt]

returnStmt = 
    do
        reserved "return"
        expr <- expression
        return $ Return expr

funCallStmt =
    do
        name <- identifier
        argExprs <- parens (sepBy expression comma)
        return $ FunCallStmt name argExprs

assignStmt = 
    do
        var <- identifier
        reservedOp "="
        expr <- expression
        return $ Assign var expr

expression = buildExpressionParser operators term

operators = [
                [Prefix (reservedOp "!" >> return (NotExpr))]
                , [Infix (reservedOp "&&" >> return (Binary AndOp)) AssocLeft]
                , [Infix (reservedOp "||" >> return (Binary OrOp)) AssocLeft]

                , [Prefix (reservedOp "-" >> return (Neg))]
                , [Infix (reservedOp "*" >> return (Binary MulOp)) AssocLeft, Infix (reservedOp "/" >> return (Binary DivOp)) AssocLeft]
                , [Infix (reservedOp "+" >> return (Binary AddOp)) AssocLeft, Infix (reservedOp "-" >> return (Binary SubOp)) AssocLeft]

                , [Infix (reservedOp "==" >> return (Binary EqOp)) AssocLeft]
                , [Infix (reservedOp ">" >> return (Binary GtOp)) AssocLeft, Infix (reservedOp "<" >> return (Binary LtOp)) AssocLeft]
            ]

funCallExpr =
    do
        name <- identifier
        argExprs <- parens (sepBy expression comma)
        return $ FunCallExpr name argExprs

ifExpr =
    do
        reserved "if"
        decisionExpr <- expression
        reserved "then"
        thenExpr <- expression
        reserved "else"
        elseExpr <- expression
        return $ If decisionExpr thenExpr elseExpr

term = parens expression 
     <|> try funCallExpr
     <|> try ifExpr
     <|> (reserved "true" >> return (Value . BoolVal $ True))
     <|> (reserved "false" >> return (Value . BoolVal $ False))
     <|> liftM Identifier identifier 
     <|> liftM (Value . IntegerVal) integer 
     <|> liftM (Value . StringVal) stringLiteral

parseStr str = 
    case parse rubbishParser "" str of
        Left e -> error (show e)
        Right r -> r

data Instruction = StoreVar String
                 | LoadVar String
                 | Push Val
                 | Pop
                 | Add
                 | Sub
                 | Mul
                 | Div
                 | FunctionInst String
                 | EndFunction
                 | Ret
                 | Call String
                 | Lt
                 | Gt
                 | Eq
                 | And
                 | Or
                 | Not
                 | Jmp Int
                 | Jtrue Int

instance Show Instruction where
    show a =
        case a of
            StoreVar str -> "storevar " ++ "\"" ++ str ++ "\""
            LoadVar str -> "loadvar " ++ "\"" ++ str ++ "\""
            Push val -> "push " ++ show val
            Pop -> "pop"
            Add -> "add"
            Sub -> "sub"
            Mul -> "mul"
            Div -> "div"
            FunctionInst str -> "#function " ++ "\"" ++ str ++ "\""
            EndFunction -> "#endfunction"
            Ret -> "ret"
            Call str -> "call " ++ "\"" ++ str ++ "\""
            Lt -> "lt"
            Gt -> "gt"
            Eq -> "eq"
            And -> "and"
            Or -> "or"
            Not -> "not"
            Jmp i -> "jmp " ++ show i
            Jtrue i -> "jtrue " ++ show i

opToInst op =
    case op of 
        AddOp -> Add
        SubOp -> Sub
        MulOp -> Mul
        DivOp -> Div
        AndOp -> And
        OrOp -> Or
        LtOp -> Lt
        GtOp -> Gt
        EqOp -> Eq

compileExpr :: Expr -> Writer [Instruction] ()
compileExpr expr =
    case expr of
        Value v -> tell [Push v]
        Identifier str -> tell [LoadVar str]
        Binary op e1 e2 -> compileExpr e1 >> compileExpr e2 >> tell [opToInst op]
        Neg e -> compileExpr e >> tell [Push . IntegerVal $ -1, Mul]
        NotExpr e -> compileExpr e >> tell [Not]
        FunCallExpr name argExprs -> mapM_ compileExpr argExprs >> tell [Call name]
        If decisionExpr thenExpr elseExpr -> 
            do
                let thenCompiled = snd . runWriter . compileExpr $ thenExpr
                let elseCompiled = snd . runWriter . compileExpr $ elseExpr
                compileExpr decisionExpr
                tell [Jtrue (2 + length elseCompiled)]
                tell elseCompiled
                tell [Jmp (1 + length thenCompiled)]
                tell thenCompiled

compileStmt :: Stmt -> Writer [Instruction] ()
compileStmt stmt =
    case stmt of
        Assign str expr -> compileExpr expr >> tell [StoreVar str, Pop]
        Seq stmtList -> mapM_ compileStmt stmtList
        FunCallStmt name argExprs -> mapM_ compileExpr argExprs >> tell [Call name, Pop]
        Return expr -> compileExpr expr >> tell [Ret]

compileFunction :: Function -> Writer [Instruction] ()
compileFunction (Function name args stmt) =
    do
        tell [FunctionInst name]
        mapM_ (\str -> tell [StoreVar str, Pop]) (reverse args)
        compileStmt stmt
        tell [EndFunction]

compileToString str = foldl (++) "" . ("#topfunction \"main\"\n":) . map (\x -> show x ++ "\n") $ (snd . runWriter . mapM_ compileFunction . parseStr $ str)
--compileAndPrint str = mapM_ print (snd . runWriter . compileStmt . parseStr $ str)

-- prog = (BinOp "*" (BinOp "+" (Identifier "a") (Identifier "b")) (BinOp "-" (Identifier "a") (Identifier "b")))