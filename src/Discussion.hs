{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (lex)

import Discussion.Token
import Discussion.Lexer

main :: IO()
main = putStrLn . show $ lex "``(\\x -> function(){ console.log(x); }) y ()"
