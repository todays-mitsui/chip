{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Discussion

main :: IO ()
main = $(defaultMainGenerator)

--------------------------------------------------------------------------------

x = Var "x"
y = Var "y"
z = Var "z"

s = VarT $ Var "S_"
k = VarT $ Var "K_"
i = VarT $ Var "I_"


case_unlambda_lambda_x_x =
  unlambda (Lambda [x] (VarT x)) @?= i

case_unlambda_lambda_x_y =
  unlambda (Lambda [x] (VarT y)) @?= App [k, VarT y]

case_unlambda_lambda_x_yx =
  unlambda (Lambda [x] (App [VarT y, VarT x])) @?= VarT y

case_unlambda_lambda_x_xy =
  unlambda (Lambda [x] (App [VarT x, VarT y])) @?= App [s, i, App [k, VarT y]]

case_unlambda_lambda_x_yz =
  unlambda (Lambda [x] (App [VarT y, VarT z])) @?= App [k, App [VarT y, VarT z]]

case_unlambda_lambda_xy_xy =
  unlambda (Lambda [x, y] (App [VarT x, VarT y])) @?= i

case_unlambda_lambda_xy_yx =
  unlambda (Lambda [x, y] (App [VarT y, VarT x]))
    @?= App [s, App[k, App[s, i]], k]

case_unlambda_var =
  unlambda (VarT x) @?= VarT x

case_unlambda_term =
  unlambda (App [Lambda [x, y] (App [VarT y, VarT x]), Lambda [z] (VarT z)])
    @?= App [App [s, App [k, App [s, i]], k], i]
