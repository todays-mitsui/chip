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

case_lex_word = Discussion.lex "foo" @?= Right [Word "foo", EOS]

case_lex_number = Discussion.lex "123" @?= Right [Number 123, EOS]

case_lex_symbol_1 = Discussion.lex "=" @?= Right [Symbol "=", EOS]

case_lex_symbol_2 = Discussion.lex "->" @?= Right [Symbol "->", EOS]

case_lex_term =
  Discussion.lex "`(function(x) { console.log(x); } y)" @?=
    Right [Backquote, LParen, Word "function", LParen, Word "x", RParen,
    LBrace, Word "console", Symbol ".", Word "log", LParen, Word "x", RParen,
    Symbol ";", RBrace, Word "y", RParen, EOS]


instance Eq ParseError where
  err1 == err2 = True

--------------------------------------------------------------------------------

case_parse_assign =
  Discussion.parse [Word "x", Symbol "=", Word "x", EOS] @?=
    Right (Assign (Var "x") [] (VarT (Var "x")))

-- Token列末に EOS が無いので失敗する
case_faild_parse_assign =
  Discussion.parse [Word "x", Symbol "=", Word "x"] @?=
    Left undefined

case_parse_reduct_1 =
  Discussion.parse [Word "x", EOS] @?=
    Right (Reduct Nothing (VarT (Var "x")))

case_parse_reduct_2 =
  Discussion.parse [Symbol ":", Number 123, Word "x", EOS] @?=
    Right (Reduct (Just 123) (VarT (Var "x")))

-- 簡約文の中に定義文の Symbol "=" を含むので失敗する
case_faild_parse_reduct_1 =
  Discussion.parse [Symbol ":", Number 123, Word "x", Symbol "=", Word "x", EOS] @?=
    Left undefined

-- 簡約文じゃないのに Number を含むので失敗する
case_faild_parse_reduct_2 =
  Discussion.parse [Number 123, Word "x", EOS] @?=
    Left undefined
