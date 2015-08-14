module Discussion.Token (Token(..)) where

data Token = Word String
             | Symbol String
             | Backquote -- "`"
             | LBrace    -- "{"
             | RBrace    -- "}"
             | LParen    -- "("
             | RParen    -- ")"
             | EOS       -- End Of Statement
             deriving (Eq, Show)

-- 述語 ----------------------------------------------------------------

isWord :: Token -> Bool
isWord (Word _) = True
isWord _        = False

isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

-----------------------------------------------------------------------
