{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.Parsec
import Data.Either
import Discussion

main :: IO ()
main = $(defaultMainGenerator)

--------------------------------------------------------------------------------

case_lexer_test1 = Discussion.lex "foo" @?= Right [Word "foo", EOS]

case_lexer_test2 = Discussion.lex "=" @?= Right [Symbol "=", EOS]

case_lexer_test3 = Discussion.lex "->" @?= Right [Symbol "->", EOS]

case_lexer_test4 =
  Discussion.lex "`(function(x) { console.log(x); } y)" @?=
  Right [Backquote, LParen, Word "function", LParen, Word "x", RParen,
  LBrace, Word "console", Symbol ".", Word "log", LParen, Word "x", RParen,
  Symbol ";", RBrace, Word "y", RParen, EOS]

instance Eq ParseError where
  err1 == err2 = show err1 == show err2

--------------------------------------------------------------------------------

-- parserTests = TestList [parserTest1]

case_parser_test1 =
  Discussion.parse [Word "x", Symbol "=", Word "x", EOS] @?=
  Right (Assign (Var "x") [] (VarT (Var "x")))

case_parser_test2 =
  assertBool "" . isLeft . Discussion.parse $ [Word "x", Symbol "=", Word "x"]
