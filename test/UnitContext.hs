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

s = createFunc [x, y, z] $ App [VarT x, VarT z, App [VarT y, VarT z]]
k = createFunc [x, y]    $ VarT x
i = createFunc [x]       $ VarT x

context = emptyContext `register` [(Var "s", s), (Var "k", k), (Var "i", i)]

--------------------------------------------------------------------------------

case_s_is_defined_in_ski = assertBool "" $ isDefined context (Var "s")

case_x_is_Undefined_in_ski = assertBool "" $ isUndefined context (Var "x")

--------------------------------------------------------------------------------

case_s_is_rank_3 = getRank context (Var "s") @?= Just 3

case_i_is_rank_1 = getRank context (Var "i") @?= Just 1

case_x_is_rank_nothing = getRank context (Var "x") @?= Nothing

case_s_is_resolved_to_just_s =
  resolve context (Var "s")
    @?= Just (Lambda [x, y, z] $ App [VarT x, VarT z, App [VarT y, VarT z]])

case_k_is_resolved_to_just_k =
  resolve context (Var "k") @?= Just (Lambda [x, y] (VarT x))

case_x_is_resolved_to_nothing = resolve context (Var "x") @?= Nothing
