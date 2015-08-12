{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.String
-- import Text.Parsec.Text
import Text.Parsec.Pos

import Control.Applicative hiding (many, (<|>))


data Token = Word String
             | Symbol String
             | Backquote -- "`"
             | LBrace    -- "{"
             | RBrace    -- "}"
             | LParen    -- "("
             | RParen    -- ")"
             deriving (Eq, Show)

isWord :: Token -> Bool
isWord (Word _) = True
isWord _        = False

isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

--------------------------------------------------------------------------------

type StParser = Parsec [Token] ()

stpAssign :: StParser [Token]
stpAssign = do
  lhs <- many1 stpWord
  stpAssignOp
  rhs <- many1 stpAnyToken
  eof
  return $ lhs ++ [Symbol "="] ++ rhs

--------------------------------------------------------------------------------

stpAnyToken :: StParser Token
stpAnyToken = satisfy' $ const True

stpWord :: StParser Token
stpWord = satisfy' isWord

stpAssignOp :: StParser Token
stpAssignOp = satisfy' $
  \tok -> case tok of
               Symbol "=" -> True
               _          -> False

--------------------------------------------------------------------------------

parseLines :: [String] -> Either ParseError [[Token]]
parseLines = mapM parseLine

parseLine :: String -> Either ParseError [Token]
parseLine = parse pTokens ""

--------------------------------------------------------------------------------

pTokens :: Parser [Token]
pTokens = many pToken <* eof

pToken :: Parser Token
pToken = token' $ choice [pWord
                        , pSymbol
                        , pBackquote
                        , pLBrace
                        , pRBrace
                        , pLParen
                        , pRParen]

--------------------------------------------------------------------------------

pWord :: Parser Token
pWord = do
  c  <- letter
  cs <- many alphaNum
  return $ Word (c:cs)

--------------------------------------------------------------------------------

pSymbol :: Parser Token
pSymbol = do
  cs <- many1 $ oneOf "!#$%&'*+,-./:;<=>?@\\^_|~"
  return $ Symbol cs

--------------------------------------------------------------------------------

pBackquote :: Parser Token
pBackquote = char '`' *> return Backquote

pLBrace :: Parser Token
pLBrace = char '{' *> return LBrace

pRBrace :: Parser Token
pRBrace = char '}' *> return RBrace

pLParen :: Parser Token
pLParen = char '(' *> return LParen

pRParen :: Parser Token
pRParen = char ')' *> return RParen

--------------------------------------------------------------------------------

token' :: Parser a -> Parser a
token' p = spaces *> p <* spaces

satisfy' :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy' f = tokenPrim (\t -> show t)
                       (\pos t _ts -> updatePosChar pos '_')
                       (\t -> if f t then Just t else Nothing)
