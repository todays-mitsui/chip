module Discussion.Eval where

import Discussion.Data

--------------------------------------------------------------------------------

-- subst v t t'
-- tの中に登場するvをt'で置き換える
subst :: Var -> Term -> Term -> Term
subst v t (App ts)    = App $ map (subst v t) ts
subst v t l@(Lambda vs t')
  | v `elem` vs       = l
  | otherwise         = Lambda vs $ subst v t t'
subst _ _ j@(Joint _) = j
subst _ _ f@(Func _)  = f
subst v t v'@(VarT v'')
  | v == v''          = Joint t
  | otherwise         = v'

removeJoint :: Term -> Term
removeJoint (App ts)      = App $ map removeJoint ts
removeJoint (Lambda vs t) = Lambda vs $ removeJoint t
removeJoint (Joint t)     = t
removeJoint t             = t

--------------------------------------------------------------------------------
