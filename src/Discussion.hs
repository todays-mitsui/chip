{-# LANGUAGE OverloadedStrings #-}

module Discussion (
    module Discussion.Data
  , module Discussion.Bool
  , module Discussion.Lexer
  , module Discussion.Parser
  , defaultMain
) where

import Discussion.Data
import Discussion.Bool
import Discussion.Lexer
import Discussion.Parser

import Prelude hiding (lex)

defaultMain = do
  let lexed  = lex "y = \\x -> \\y -> \\z -> ``z y x"
  putStrLn $ show lexed
  putStrLn ""
  let parsed = parse =<< lexed
  putStrLn $ show parsed
