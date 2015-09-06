module Discussion.Converter (compact, unlambda) where

import Control.Applicative ((<$>))

import Discussion.Data
import Discussion.Bool

--------------------------------------------------------------------------------

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

-- Termの中からLambda式を完全に取り除く
-- Lambda式は同等の振る舞いをするSKIコンビネータ-の組に置き換えられる
unlambda :: Term -> Term
unlambda (App ts)    = App $ fmap unlambda ts
unlambda (Lambda vs t) = unabstract vs t
unlambda t             = t

-- Termの中から[Var]を取り除く
-- [Var]はLambdaの引数にあたるリストでunabstractは引数を内側から処理するため
-- 引数のリストをreverseして、実際の処理はunabstract'に委託
unabstract :: [Var] -> Term -> Term
unabstract vs t = compact $ unabstract' (reverse vs) t

unabstract' :: [Var] -> Term -> Term
unabstract' []     t = t
unabstract' (v:vs) t = unabstract vs $ unabstractOnce v t

unabstractOnce :: Var -> Term -> Term
unabstractOnce v t@(App [t1, t2])
  | v `isFreeIn` t = App [k, t]
  | v `isFreeIn` t1 && t2 == VarT v
                   = t1
  | otherwise      = App [s, unabstractOnce v t1, unabstractOnce v t2]
unabstractOnce v t@(App ts)
  | v `isFreeIn` t = App [k, t]
  | v `isFreeIn` App (init ts) && last ts == VarT v
                   = App (init ts)
  | otherwise      = App [s, unabstractOnce v $ App (init ts), unabstractOnce v $ last ts]
unabstractOnce v (Lambda vs t) = unabstractOnce v $ unabstract vs t
unabstractOnce v t@(VarT v')
  | v == v'        = i
  | otherwise      = App [k, t]
unabstractOnce v t@(Func _ _)
                   = App [k, t]

--------------------------------------------------------------------------------

s = Func 3 "S_"
k = Func 2 "K_"
i = Func 1 "I_"
