module Parser.Parser (whileParser) where

import Control.Monad
import Text.ParserCombinators.Parsec (letter, alphaNum, Parser, sepBy1, (<|>), try)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Parser.Ast (Statement (..), AExpr (..), BExpr (..), Aop (..), Bop (..), RelOp (..), OpAssOp (..))
--import Eval.EvalAexpr (evalAexpr)

languageDef =
   emptyDef { Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "skip"
                                      , "true"
                                      , "false"
                                      , "until"
                                      , "repeat"
                                      , "for"
                                      , "to"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", ":="
                                      , "<", ">", "&&", "||", "!"
                                      , "<=", ">=", "==", "!="
                                      , "-=", "*=", "+=", ";"
                                      , "(", ")", ","
                                      ]
            }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Statement
whileParser = whiteSpace >> statement

statement :: Parser Statement
statement =   parens statement
           <|> sequenceOfStmt

sequenceOfStmt =
   do list <- sepBy1 statement' semi
      -- If there's only one statement return it without using Seq.
      return $ if length list == 1 then head list else Composition list

statement' :: Parser Statement
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> repeatStmt
           <|> forStmt
           <|> try opAssignStmt
           <|> try pairAssignStmt
           <|> assignStmt

ifStmt :: Parser Statement
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     Conditional cond stmt1 <$> statement

whileStmt :: Parser Statement
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     While cond <$> statement

repeatStmt :: Parser Statement
repeatStmt =
  do reserved "repeat"
     stmt <- statement
     reserved "until"
     Repeat stmt <$> bExpression

forStmt :: Parser Statement
forStmt =
  do reserved "for"
     i <- identifier
     reservedOp ":="
     to <- aExpression
     reserved "to"
     from <- aExpression
     reserved "do"
     For i to from <$> statement

assignStmt :: Parser Statement
assignStmt =
  do var  <- identifier
     reservedOp ":="
     Assignment var <$> aExpression

pairAssignStmt :: Parser Statement
pairAssignStmt =
  do var1  <- identifier
     reservedOp ","
     var2  <- identifier
     reservedOp ":="
     expr1 <- aExpression
     reservedOp ","
     expr2 <- aExpression
     return $ PairAssignment (var1, var2) (expr1, expr2)

opAssignStmt :: Parser Statement
opAssignStmt =
  do var  <- identifier
     op <- opOperators
     OpAssignment op var <$> aExpression

skipStmt :: Parser Statement
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Infix  (reservedOp "*"   >> return (ABinOp Mult)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinOp Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinOp Minus)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "!" >> return BUnOp)          ]
             , [Infix  (reservedOp "&&" >> return (BBinOp OpAnd     )) AssocLeft,
                Infix  (reservedOp "||"  >> return (BBinOp OpOr      )) AssocLeft]
             ]

opOperators = (reservedOp "*=" >> return Multeq)
          <|> (reservedOp "+=" >> return Inceq)
          <|> (reservedOp "-=" >> return Deceq)

relation =   (reservedOp ">" >> return Gg)
          <|> (reservedOp "<" >> return Ll)
          <|> (reservedOp "<=" >> return Leq)
          <|> (reservedOp ">=" >> return Geq)
          <|> (reservedOp "==" >> return Equ)
          <|> (reservedOp "!=" >> return Neq)

aTerm =  parens aExpression
      <|> liftM Var identifier
      <|> liftM AConst integer

bTerm =  parens bExpression
      <|> (reserved "true"  >> return (BConst True ))
      <|> (reserved "false" >> return (BConst False))
      <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     BoolRelation op a1 <$> aExpression
