{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Discussion.Parser (parse) where

--------------------------------------------------------------------------------

import           Text.Parsec         hiding        (parse, count)
import qualified Text.Parsec         as     Parsec (parse)
import           Text.Parsec.Pos
import           Control.Applicative hiding        (many, (<|>))

import           Discussion.Data

--------------------------------------------------------------------------------

parse = Parsec.parse expr ""

--------------------------------------------------------------------------------

type TokParser = Parsec [Token] ()

expr :: TokParser Expr
expr = assign <|> reduct

assign :: TokParser Expr
assign = do
  (v, args) <- simpleApply
  tok $ Symbol "="
  rhs       <- term
  tok $ EOS
  return $ Assign v args (compact rhs)

reduct :: TokParser Expr
reduct = do
  n <- count
  t <- term
  tok $ EOS
  return $ Reduct n t

-- TokParser Term --------------------------------------------------------------

term :: TokParser Term
term = parens factor

factor :: TokParser Term
factor = app <|> lambda <|> varT

--------------------------------------------------------------------------------

varT :: TokParser Term
varT = VarT <$> var

--------------------------------------------------------------------------------

app :: TokParser Term
app = appLikeLazyK

appLikeLazyK :: TokParser Term
appLikeLazyK = do
  tok Backquote
  t1 <- term
  t2 <- term
  return $ App [t1, t2]

appLikeHaskell :: TokParser Term
appLikeHaskell = undefined

--------------------------------------------------------------------------------

lambda :: TokParser Term
lambda = lambdaLikeHaskell

lambdaLikeHaskell :: TokParser Term
lambdaLikeHaskell = do
  tok $ Symbol "\\"
  vs <- many1 var
  tok $ Symbol "->"
  t  <- term
  return $ Lambda vs t

-- TokParser Var ---------------------------------------------------------------

-- "``foo bar buz"の形のToken列をparseして
-- (Var "foo", [Var "bar", Var "buz"])を返す
simpleApply :: TokParser (Var, [Var])
simpleApply = do
  vs <- simpleApply'
  let (v:args) = reverse $ vs
  return (v, args)

-- "``foo bar buz"の形のToken列をparseする
-- 返される[Var]は逆順になっているので注意
simpleApply' :: TokParser [Var]
simpleApply' = do
    tok Backquote
    vs <- simpleApply'
    v  <- var
    return (v:vs)
  <|> (: []) <$> var

var :: TokParser Var
var = word2var <$> word

--------------------------------------------------------------------------------

count :: TokParser (Maybe Int)
count = do
    tok $ Symbol ":"
    Number n <- number
    return $ Just n
  <|> return Nothing

-- TokParser Token -------------------------------------------------------------

word :: TokParser Token
word = satisfy' isWord

number :: TokParser Token
number = satisfy' isNumber

-- anyToken :: TokParser Token
-- anyToken = satisfy' $ const True

tok :: Token -> TokParser Token
tok t = satisfy' (t ==)

parens :: TokParser a -> TokParser a
parens p = (tok LParen *> p <* tok RParen) <|> p

satisfy' :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy' f = tokenPrim (\t -> show t)
                       (\pos t _ts -> updatePosChar pos '_')
                       (\t -> if f t then Just t else Nothing)

--------------------------------------------------------------------------------

word2var :: Token -> Var
word2var (Word v) = Var v
word2var _        = Var "_"
