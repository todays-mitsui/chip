module Discussion.Bool where

import Discussion.Data

isWord :: Token -> Bool
isWord (Word _) = True
isWord _        = False

isNumber :: Token -> Bool
isNumber (Number _) = True
isNumber _          = False

isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

--------------------------------------------------------------------------------

isBoundIn :: Var -> Term -> Bool
v `isBoundIn` (App ts)   = any (isBoundIn v) ts
v `isBoundIn` (Lambda vs t)
  | v `elem` vs          = False
  | otherwise            = v `isBoundIn` t
_ `isBoundIn` (Func _ _) = False
v `isBoundIn` (VarT v')  = v == v'

isFreeIn :: Var -> Term -> Bool
isFreeIn v = not . isBoundIn v

--------------------------------------------------------------------------------

-- 閉式判定
-- 閉式 = 外部で束縛された変数を持ち込んでいない式
isClosed :: Term -> Bool
isClosed = isClosed' []

isClosed' :: [Var] -> Term -> Bool
isClosed' vs (App ts)       = all (isClosed' vs) ts
isClosed' vs (Lambda vs' t) = isClosed' (vs ++ vs') t
isClosed' _  (Func _ _)     = True
isClosed' vs (VarT v)       = v `elem` vs

--------------------------------------------------------------------------------
