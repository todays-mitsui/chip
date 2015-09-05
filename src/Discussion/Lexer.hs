{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Discussion.Lexer (lex) where

--------------------------------------------------------------------------------

import Prelude hiding (lex)

import Text.Parsec
import Text.Parsec.String
-- import Text.Parsec.Text
import Text.Parsec.Pos

import Control.Applicative hiding (many, (<|>))

import Discussion.Data
import Discussion.Bool

--------------------------------------------------------------------------------

lex :: String -> Either ParseError [Token]
lex src = joinEOS <$> (parseLines . lines $ src)

--------------------------------------------------------------------------------

-- Token列の適切な場所にEOSを差し込みながらToken列のリストをconcatする
-- 次の行のToken列がSymbol "="を含めば、現在のToken列の末尾にEOSを挿入
-- 最後のToken列の末尾にもEOSを足す
joinEOS :: [[Token]] -> [Token]
joinEOS []        = []
joinEOS (toks:[]) = toks ++ [EOS]
joinEOS (toks1:toks2:contTokss) =
  if Symbol "=" `elem` toks2
     then joinEOS $ (toks1 ++ [EOS] ++ toks2) : contTokss
     else joinEOS $ (toks1 ++ toks2) : contTokss

--------------------------------------------------------------------------------

parseLines :: [String] -> Either ParseError [[Token]]
parseLines = mapM parseLine

parseLine :: String -> Either ParseError [Token]
parseLine = parse pTokens ""

--------------------------------------------------------------------------------

-- Token列を取り出す
-- ストリームを使いきらずに途中でTokenのparseに失敗した場合は
-- Token列のparse全体が失敗する
pTokens :: Parser [Token]
pTokens = many pToken <* eof

pToken :: Parser Token
pToken = token' $ choice [pWord
                        , pNumber
                        , pSymbol
                        , pBackquote
                        , pLBrace
                        , pRBrace
                        , pLParen
                        , pRParen]

--------------------------------------------------------------------------------

-- /\w(\w|\d)+/
pWord :: Parser Token
pWord = Word <$> ((:) <$> letter <*> many alphaNum)

pNumber :: Parser Token
pNumber = Number . read <$> many1 digit

--------------------------------------------------------------------------------

-- /[!#$%&'*+,-.\/:;<=>?@\\^_|~]+/
pSymbol :: Parser Token
pSymbol = Symbol <$> (many1 . oneOf $ "!#$%&'*+,-./:;<=>?@\\^_|~")

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

-- 前後の空白をtrim
token' :: Parser a -> Parser a
token' p = spaces *> p <* spaces
