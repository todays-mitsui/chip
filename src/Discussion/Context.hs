module Discussion.Context where

import           Control.Applicative        ((<$>))

import qualified Data.Map.Lazy        as ML
import           Data.Map.Lazy              (Map)

import           Discussion.Data
import           Discussion.Converter

--------------------------------------------------------------------------------

type Context = Map Var Func
data Func    = Func Rank Term deriving (Eq, Show)
type Rank    = Int

--------------------------------------------------------------------------------

emptyContext :: Context
emptyContext = ML.empty

--------------------------------------------------------------------------------

createFunc :: Args -> Term -> Func
createFunc args t = Func (length args) (compact $ Lambda args t)

register :: Context -> [(Var,Func)] -> Context
register c fs = foldl insert c fs
  where c `insert` (v, f) = ML.insert v f c

--------------------------------------------------------------------------------

isDefined :: Context -> Var -> Bool
isDefined c v = ML.member v c

isUndefined :: Context -> Var -> Bool
isUndefined c v = not $ isDefined c v

--------------------------------------------------------------------------------

getRank :: Context -> Var -> Maybe Rank
getRank c v = getRank' <$> ML.lookup v c
  where getRank' (Func r _) = r

resolve :: Context -> Var -> Maybe Term
resolve c v = getTerm <$> ML.lookup v c
  where getTerm (Func _ t) = t
