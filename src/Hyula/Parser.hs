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
      [ try $ do
          f <- op
          x <- ele
          go (f acc x)
      , return acc
      ]

reduce_right :: Parser b -> Parser a -> Parser (a -> b -> b) -> Parser b
reduce_right start ele op = p
 where
  p = do
    choice
      [ try $ do
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
        , L.skipLineComment "# " <?> "line comment"
        , lex "#_" *> pExp $> () <?> "AST comment"
        , nestedBlockComment "#|" "|#" <?> "block comment"
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
  _ <- "'"
  c <-
    choice
      [ "\\" *> choice [char '\\', char '\'']
      , anySingle
      ]
  _ <- "'"
  return c

pAtom :: Parser Atom
pAtom =
  lex
    $ choice
      [ F64 <$> try L.float <?> "F64 atom"
      , I64 <$> L.decimal <?> "I64 atom"
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

wrapScoped :: [Exp] -> Exp
wrapScoped es = (Seq (SeqProp{scoped = True}) es)

pIf :: Parser Exp
pIf = do
  _ <- kw "if"
  cond <- pExp
  let kw_else = kw "else"
  then_branch <- pBlockAlt kw_else kw_else
  else_branch <- pBlock (kw "end")
  return (If cond (wrapScoped then_branch) (wrapScoped else_branch))

pSeq :: Parser Exp
pSeq = do
  _ <- kw "do"
  body <- pBlock (kw "end")
  return (wrapScoped body)

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
pSeqExp = do
  lst <- sepList (lex ";") pMatch
  let result = case lst of
        hd :| [] -> hd
        hd :| rst -> Seq (SeqProp{scoped = False}) (hd : rst)
  (return result)

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

pUnioned :: Parser Pat -- TODO: maybe need to preserve '|' for union type
pUnioned = check_union <$> sepList (lex "|") pPatPrimary
 where
  check_union eles = case eles of
    e :| [] -> e
    u -> Union u

pPat :: Parser Pat
pPat = pUnioned

-- sepList :: Parser a -> Parser e -> Parser (NonEmpty e)

-- Top Level
pTopLevel :: Parser Exp
pTopLevel = do
  _ <- sc
  _ <- nl_star
  body <- sepList nl_plus pExp
  _ <- nl_star
  _ <- eof
  let result = case body of
        stmt :| [] -> stmt
        stmt :| stmts -> Seq (SeqProp{scoped = False}) (stmt : stmts)
  (return result)
