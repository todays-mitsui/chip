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

s = Lambda [x, y, z] $ App [VarT x, VarT z, App [VarT y, VarT z]]
k = Lambda [x, y]    $ VarT x

i = App [s, k, k]

r = root i

--------------------------------------------------------------------------------

case_1st_go_app_left = goAppL r @?= Just (App [s, k], [AppRCrumb [k]])

case_then_2nd_go_app_left =
  goAppL (App [s, k], [AppRCrumb [k]])
    @?= Just (s , [AppRCrumb [k, k]])

case_then_3rd_go_into_lambda =
  goIntoLambda (s , [AppRCrumb [k, k]])
    @?= Just (App [VarT x, VarT z, App [VarT y, VarT z]], [LambdaCrumb [x, y, z], AppRCrumb [k, k]])

case_then_4th_go_app_right =
  goAppR (App [VarT x, VarT z, App [VarT y, VarT z]], [LambdaCrumb [x, y, z], AppRCrumb [k, k]])
    @?= Just (App [VarT y, VarT z], [AppLCrumb [VarT x, VarT z], LambdaCrumb [x, y, z], AppRCrumb [k, k]])

case_then_5th_go_into_joint =
  goIntoJoint (App [VarT y, VarT z], [AppLCrumb [VarT x, VarT z], LambdaCrumb [x, y, z], AppRCrumb [k, k]])
    @?= Nothing

case_once_again_5th_go_app_left =
  goAppL (App [VarT y, VarT z], [AppLCrumb [VarT x, VarT z], LambdaCrumb [x, y, z], AppRCrumb [k, k]])
    @?= Just (VarT y, [AppRCrumb [VarT z], AppLCrumb [VarT x, VarT z], LambdaCrumb [x, y, z], AppRCrumb [k, k]])

case_then_go_root =
  goRoot (VarT y, [AppRCrumb [VarT z], AppLCrumb [VarT x, VarT z], LambdaCrumb [x, y, z], AppRCrumb [k, k]])
    @?= Just r

case_last_get_root =
  getRoot (VarT y, [AppRCrumb [VarT z], AppLCrumb [VarT x, VarT z], LambdaCrumb [x, y, z], AppRCrumb [k, k]])
    @?= i
