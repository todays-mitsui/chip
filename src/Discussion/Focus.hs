module Discussion.Focus where

import           Discussion.Data

--------------------------------------------------------------------------------

type Focus       = (Term, Breadcrumbs)
type Breadcrumbs = [Crumb]
data Crumb       = AppRCrumb Term
                 | AppLCrumb [Term]
                 | LambdaCrumb Args
                 | JointCrumb
                   deriving (Eq, Show)

--------------------------------------------------------------------------------

goAppL :: Focus -> Maybe Focus
goAppL (App (t1:t2:[]), bs) = Just (t1           , AppRCrumb t2        : bs)
goAppL (App ts        , bs) = Just (App (init ts), AppRCrumb (last ts) : bs)
goAppL _                    = Nothing

goAppR :: Focus -> Maybe Focus
goAppR (App ts, bs) = Just (last ts, AppLCrumb (init ts) : bs)
goAppR _            = Nothing

goIntoLambda :: Focus -> Maybe Focus
goIntoLambda (Lambda vs t, bs) = Just (t, LambdaCrumb vs : bs)
goIntoLambda _                 = Nothing

goIntoJoint :: Focus -> Maybe Focus
goIntoJoint (Joint t, bs) = Just (t, JointCrumb : bs)
goIntoJoint _             = Nothing

goUp :: Focus -> Maybe Focus
goUp (App ts, AppRCrumb t    : bs) = Just (App (ts ++ [t]), bs)
goUp (t1    , AppRCrumb t2   : bs) = Just (App [t1, t2]   , bs)
goUp (t     , AppLCrumb ts   : bs) = Just (App (ts ++ [t]), bs)
goUp (t     , LambdaCrumb vs : bs) = Just (Lambda vs t    , bs)
goUp (t     , JointCrumb     : bs) = Just (Joint t        , bs)
goUp (_     , [])                  = Nothing

goRoot :: Focus -> Maybe Focus
goRoot (t, []) = Just (t, [])
goRoot f       = goRoot =<< goUp f

root :: Term -> Focus
root t = (t, [])

getRoot :: Focus -> Term
getRoot f = let Just (t, []) = goRoot f
            in  t
