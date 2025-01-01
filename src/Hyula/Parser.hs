{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hyula.Parser where

import Hyula.Ast.Base (Atom (..))
import Hyula.Ast.Raw (Exp (..), Pat (..), SeqProp (..))

import Data.Char (isAlpha, isAlphaNum)
import Data.Text qualified as T
import Relude hiding (Any, many)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- Useful Combinators

reduce_left :: Parser a -> Parser b -> Parser (a -> b -> a) -> Parser a
reduce_left start ele op =
  start >>= go
 where
  go acc =
    choice
      [ do
          f <- op
          x <- ele
          return (f acc x)
      , return acc
      ]

reduce_right :: Parser b -> Parser a -> Parser (a -> b -> b) -> Parser b
reduce_right start ele op = p
 where
  p = do
    choice
      [ do
          e <- ele
          o <- op
          rest <- p
          return (o e rest)
      , start
      ]

groupedList :: Parser a -> Parser b -> Parser c -> Parser e -> Parser [e]
groupedList lsep rsep sep element =
  lsep *> list_rest
 where
  list_rest =
    choice
      [ rsep $> []
      , (:)
          <$> element
          <*> ( choice
                  [ rsep $> []
                  , sep *> list_rest
                  ]
              )
      ]

sepList :: Parser a -> Parser e -> Parser (NonEmpty e)
sepList sep element =
  p
 where
  p = do
    e <- element
    rest <- optional (sep *> p)
    let result = case rest of
          Nothing -> e :| []
          Just (lst0 :| lst) -> e :| (lst0 : lst)
    (return result)

-- Lexing

nestedBlockComment :: Text -> Text -> Parser ()
nestedBlockComment start end = void $ label "nested block comment" $ go
 where
  go = do
    _ <- string start
    manyTill (choice [void go, void anySingle]) (string end)

sc :: Parser ()
sc =
  skipMany
    ( choice
        [ satisfy (`elem` [' ', '\r', '\t']) $> () <?> "whitespace"
        , (L.skipLineComment "#" <* notFollowedBy (choice ["|", "_"])) <?> "line comment"
        , lex "#_" *> pExp $> () <?> "AST comment"
        , L.skipBlockComment "#|" "|#" <?> "block comment"
        ]
    )

lex :: Parser a -> Parser a
lex = L.lexeme sc

kw :: Text -> Parser Text
kw keyword = lex (string keyword <* notFollowedBy alphaNumChar)

nl :: Parser ()
nl = lex "\n" $> ()

nl_star :: Parser ()
nl_star = many nl $> ()

nl_plus :: Parser ()
nl_plus = nl *> nl_star

-- Atoms

pBool :: Parser Bool
pBool =
  choice
    [ True <$ kw "true"
    , False <$ kw "false"
    ]

pChar :: Parser Char
pChar = do
  _ <- char '\''
  c <-
    choice
      [ satisfy $ not . (`elem` ['\\', '\''])
      , char '\\' *> choice [char '\\', char '\'']
      ]
  _ <- char '\''
  return c

pAtom :: Parser Atom
pAtom =
  choice
    [ I64 <$> L.decimal <?> "I64 atom"
    , F64 <$> L.float <?> "F64 atom"
    , Bool <$> pBool <?> "Bool atom"
    , Char <$> pChar <?> "Char atom"
    , Unit <$ string "()"
    ]

-- Expressions

pSymbol :: Parser Exp
pSymbol =
  lex
    $ (Var . T.pack)
    <$> ( (:)
            <$> satisfy (\c -> isAlpha c || c == '_')
            <*> many (satisfy (\c -> isAlphaNum c || c == '_'))
        )

pBlock :: Parser a -> Parser [Exp]
pBlock block_terminator =
  choice
    [ lex ":" *> (pure <$> pExp)
    , nl_plus *> manyTill (pExp <* nl_plus) block_terminator
    ]

pBlockAlt :: Parser a -> Parser b -> Parser [Exp]
pBlockAlt exp_terminator block_terminator =
  choice
    [ lex ":" *> (pure <$> pExp) <* exp_terminator
    , nl_plus *> manyTill (pExp <* nl_plus) block_terminator
    ]

pIf :: Parser Exp
pIf = do
  _ <- kw "if"
  cond <- pExp
  let kw_else = kw "else"
  then_branch <- pBlockAlt kw_else kw_else
  else_branch <- pBlock (kw "end")
  return (If cond then_branch else_branch)

pSeq :: Parser Exp
pSeq = do
  _ <- kw "do"
  body <- pBlock (kw "end")
  return (Seq (SeqProp{scoped = True}) body)

pList :: Parser Exp
pList = List <$> groupedList (lex "[") (lex "]") (lex ",") pExp

pFunctionCall :: Parser (Exp, [Exp])
pFunctionCall = do
  fn_name <- pSymbol
  arg_list <- groupedList (lex "(") (lex ")") (lex ",") pExp
  return (fn_name, arg_list)

pPrimary :: Parser Exp
pPrimary =
  choice
    [ pList
    , (lex "(") *> pExp <* (lex ")")
    , try ((\(f, args) -> Call f args) <$> pFunctionCall)
    , try (Atom <$> pAtom)
    , pSymbol
    ]

pUFCS :: Parser Exp
pUFCS =
  reduce_left
    pPrimary
    pFunctionCall
    ( lex "." *> return (\inner (f, args) -> Call f (inner : args))
    )

pMatch :: Parser Exp
pMatch =
  reduce_right pUFCS pPat (lex "=" *> return (\pat rhs -> Match pat rhs))

pSeqExp :: Parser Exp
pSeqExp =
  Seq (SeqProp{scoped = False}) . toList <$> sepList (lex ";") pMatch

pExpLike :: Parser Exp
pExpLike = pSeqExp

pExp :: Parser Exp
pExp =
  choice
    [ pSeq
    , pIf
    , pExpLike
    ]

-- Patterns

pLit :: Parser Pat
pLit = Lit <$> pAtom

pAny :: Parser Pat
pAny = Any <$ kw "_"

pPatList :: Parser Pat
pPatList = PatList <$> groupedList (lex "[") (lex "]") (lex ",") pPat

pPatPrimary :: Parser Pat
pPatPrimary = choice [pLit, pAny, pPatList]

pUnioned :: Parser Pat
pUnioned = Union <$> sepList (lex "|") pPatPrimary

pPat :: Parser Pat
pPat = pUnioned

-- Top Level
pTopLevel :: Parser Exp
pTopLevel = Seq (SeqProp{scoped = False}) <$> (nl_star *> pBlock eof)
