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

case_x_is_bound_in_x = assertBool "" $ x `isBoundIn` VarT x

case_x_is_free_in_y = assertBool "" $ x `isFreeIn` VarT y

case_x_is_bound_in_xx =
  assertBool "" $ x `isBoundIn` App [VarT x, VarT x]

case_x_is_bound_in_xy =
  assertBool "" $ x `isBoundIn` App [VarT x, VarT y]

case_x_is_free_in_yy =
  assertBool "" $ x `isFreeIn` App [VarT y, VarT y]

case_x_is_bound_in_lambda_y_x =
  assertBool "" $ x `isBoundIn` Lambda [y] (VarT x)

case_x_is_free_in_lambda_x_x =
  assertBool "" $ x `isFreeIn` Lambda [x] (VarT x)

case_x_is_bound_in_term =
  assertBool "" $ x `isBoundIn` App [VarT z, Lambda [y] (App [VarT y, VarT x]), VarT y]

case_x_is_free_in_term =
  assertBool "" $ x `isFreeIn` App [VarT z, Lambda [x] (App [VarT y, VarT x]), VarT y]

--------------------------------------------------------------------------------

case_x_is_not_closed =
  assertBool "" . not . isClosed $ VarT x

case_xy_is_not_closed =
  assertBool "" . not . isClosed $ App [VarT x, VarT y]

case_lambda_x_x_is_closed =
  assertBool "" . isClosed $ Lambda [x] (VarT x)

case_lambda_x_y_is_not_closed =
  assertBool "" . not . isClosed $ Lambda [x] (VarT y)

case_lambda_xy_x_is_closed =
  assertBool "" . isClosed $ Lambda [x, y] (App [VarT x])

case_lambda_xy_yx_is_closed =
  assertBool "" . isClosed $ Lambda [x, y] (App [VarT y, VarT x])

case_lambda_xy_zx_is_not_closed =
  assertBool "" . not . isClosed $ Lambda [x, y] (App [VarT z, VarT x])
