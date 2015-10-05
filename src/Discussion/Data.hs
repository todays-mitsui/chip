module Discussion.Data where

--------------------------------------------------------------------------------

data Term = VarT Var
            | App [Term]
            | Lambda [Var] Term
            | Joint Term           -- Termに対する処理の途中で目印として使う事がある
            deriving (Eq, Show)

newtype Var = Var Identifier
              deriving (Eq, Ord, Show)

type Identifier = String

--------------------------------------------------------------------------------

data Expr = Assign Var Args Term
            | Reduct (Maybe Int) Term
            deriving (Eq, Show)

type Args  = [Var]
type Count = Int

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
