
data Expr = Assign Var Args Term
            | Reduct Count Term

type Args  = [Var]
type Count = Int

--------------------------------------------------------------------------------

data Term = VarT Var
            | Apply [Var] [Term]
            | Lambda [Var] Term
            | Func Rank Identifier
            deriving (Eq, Show)

newtype Var = Var Identifier
              deriving (Eq, Show)

type Identifier = String
type Rank       = Int

--------------------------------------------------------------------------------
