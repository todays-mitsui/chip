module Discussion.Data where

data Expr = Assign Var Args Term
            | Reduct (Maybe Int) Term
            deriving (Eq, Show)

type Args  = [Var]
type Count = Int

--------------------------------------------------------------------------------

data Term = VarT Var
            | App [Term]
            | Lambda [Var] Term
            | Func Rank Identifier
            deriving (Eq, Show)

newtype Var = Var Identifier
              deriving (Eq, Show)

type Rank       = Int


--------------------------------------------------------------------------------

data Token = Word Identifier
             | Number Int
             | Symbol Identifier
             | Backquote -- "`"
             | LBrace    -- "{"
             | RBrace    -- "}"
             | LParen    -- "("
             | RParen    -- ")"
             | EOS       -- End Of Statement
             deriving (Eq, Show)

type Identifier = String
