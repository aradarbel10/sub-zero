{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Surface.Parse ( parseExpr, parseType ) where

import Text.Parsec hiding ( runParser )
import Text.ParserCombinators.Parsec hiding (Parser, runParser, parsem, try)
import Text.Parsec.Indent

import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Language as L

import Data.Functor.Identity(Identity)
import Data.Functor (($>))

import Surface.AST ( Expr(..), Type(..), Name(..) )


type Parser a = ParsecT String () (IndentT Identity) a


--- Lexing ---
lexer :: T.GenTokenParser String u (IndentT Identity)
lexer = T.makeTokenParser $ T.LanguageDef {
    T.commentStart = "{-",
    T.commentEnd = "-}",
    T.commentLine = "--",
    T.nestedComments = True,

    T.identStart  = letter,
    T.identLetter = alphaNum <|> char '-',
    T.opStart  = oneOf "!@#$%^&*-=+{}<>[]|/.,;:→↦",
    T.opLetter = oneOf "!@#$%^&*-=+{}<>[]|/.,;:",

    T.reservedNames = [
        "let", "in",
        "λ", "fun",
        "Nat", "ℕ", "Int", "ℤ", "Real", "ℝ",
        "Bool", "𝔹", "True", "False"
    ],
    T.reservedOpNames = [
        "=", ".", ":", ";",
        "->", "→", "↦"
    ],

    T.caseSensitive = True

    }


identifier :: Parser String
identifier = T.identifier lexer
natural :: Parser Integer
natural = T.natural lexer
integer :: Parser Integer
integer = T.integer lexer
float :: Parser Double
float = T.float lexer


parens :: Parser a -> Parser a
parens = T.parens lexer
braces :: Parser a -> Parser a
braces = T.braces lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer
reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

--- Parsing API ---
runParser :: Parser a -> String -> Either String a
runParser p s = case runIndentParser p () "" s of
    Left err -> Left $ show err
    Right res -> Right res

parseExpr :: String -> Either String Expr
parseExpr = runParser exprParser

parseType :: String -> Either String Type
parseType = runParser typeParser

typeParser :: Parser Type
typeParser = E.buildExpressionParser table typeAtomParser
    where
    table :: [[E.Operator String () (IndentT Identity) Type]]
    table = [
        [E.Infix ((reservedOp "->" <|> reservedOp "→") >> return Arrow) E.AssocRight]]


--- Expression Parsing ---
exprParser :: Parser Expr
exprParser =
        try (do
        reserved "fun" <|> reserved "λ"
        name <- identifier
        reservedOp "."
        body <- exprParser
        pure $ Lam (N name) body
        )
    <|> try (do
        name <- identifier
        reservedOp "↦"
        body <- exprParser
        pure $ Lam (N name) body
        )
    <|> try (do
        reserved "let"
        name <- identifier
        reservedOp "="
        body <- exprParser
        reserved "in"
        rest <- exprParser
        pure $ Let (N name) body rest
        )
    <|> try (E.buildExpressionParser table juxtaParser)

    where
    table :: [[E.Operator String () (IndentT Identity) Expr]]
    table = [
        [E.Postfix (try $ do
            char '.'
            lbl <- identifier
            pure $ \e -> Proj e (N lbl))]
        ]

juxtaParser :: Parser Expr
juxtaParser = do
    t <- withPos atomParser
    rest <- many (sameOrIndented >> atomParser)
    return $ if null rest
        then t
        else foldl App t rest

atomParser :: Parser Expr
atomParser =
        try (Var . N <$> identifier)
    <|> try (RealLit <$> float)
    <|> try (NatLit <$> natural)
    <|> try (IntLit <$> integer)
    <|> try (reserved "True" $> BoolLit True)
    <|> try (reserved "False" $> BoolLit False)
    <|> try (parens exprParser)
    <|> try (braces $ Record <$> sepBy (do
        lbl <- identifier
        reservedOp "="
        val <- exprParser
        pure (N lbl, val)
        ) (reservedOp ";"))

--- Type Parsing ---
typeAtomParser :: Parser Type
typeAtomParser =
        ((reserved "Nat"  <|> reserved "ℕ") >> pure Nat)
    <|> ((reserved "Int"  <|> reserved "ℤ") >> pure Int)
    <|> ((reserved "Real" <|> reserved "ℝ") >> pure Real)
    <|> ((reserved "Bool" <|> reserved "𝔹") >> pure Bool)
    <|> braces (RecordType <$> sepBy (do
        lbl <- identifier
        reservedOp ":"
        typ <- typeParser
        pure (N lbl, typ)
        ) (reservedOp ";"))