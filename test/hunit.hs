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

case_lexer_test5 = Discussion.lex ":123" @?= Right [Symbol ":", Number 123, EOS]

instance Eq ParseError where
  err1 == err2 = True

--------------------------------------------------------------------------------

-- parserTests = TestList [parserTest1]

case_parser_test1 =
  Discussion.parse [Word "x", Symbol "=", Word "x", EOS] @?=
    Right (Assign (Var "x") [] (VarT (Var "x")))

-- Token列末に EOS が無いので失敗する
case_parser_test2 =
  Discussion.parse [Word "x", Symbol "=", Word "x"] @?=
    Left undefined

case_parser_test3 =
  Discussion.parse [Symbol ":", Number 123, Word "x", EOS] @?=
    Right (Reduct (Just 123) (VarT (Var "x")))

-- 簡約文の中に定義文の Symbol "=" を含むので失敗する
case_parser_test4 =
  Discussion.parse [Symbol ":", Number 123, Word "x", Symbol "=", Word "x", EOS] @?=
    Left undefined

-- 簡約文じゃないのに Number _ を含むので失敗する
case_parser_test5 =
  Discussion.parse [Number 123, Word "x", EOS] @?=
    Left undefined
