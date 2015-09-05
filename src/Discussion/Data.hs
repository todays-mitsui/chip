module Discussion.Data where

import Control.Applicative ((<$>))

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

-- Termを本質的な構造は変えずに最小の構成にする
compact :: Term -> Term
compact v@(VarT _)                  = v
compact f@(Func _ _)                = f
compact (App ((App ts1) : ts2))     =
  compact $ App ((compact <$> ts1) ++ (compact <$> ts2))
compact (App   ts)                  = App (compact <$> ts)
compact (Lambda vs1 (Lambda vs2 t)) = compact $ Lambda (vs1 ++ vs2) (compact t)
compact (Lambda vs   t)             = Lambda vs (compact t)

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
