module Discussion.Term where

data Expr = Assign Var Args Term
            | Reduct Term
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

type Identifier = String
type Rank       = Int

--------------------------------------------------------------------------------
