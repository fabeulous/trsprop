module Properties
    ( properties
    ) where

import Data.Rewriting.CriticalPair as CP
import Data.Rewriting.Rules hiding (map)
import Data.Rewriting.Rule (Rule)
import qualified Data.Rewriting.Rule as R
import Data.Rewriting.Term (Term (..))
import qualified Data.Rewriting.Term as T


properties :: [(String, [Rule String String] -> Bool)]
properties =
    [ ("shallow", isShallow)
    , ("flat", isFlat)
    , ("ground", isGround)
    , ("left-ground", isLeftGround)
    , ("right-ground", isRightGround)
    , ("linear", isLinear)
    , ("left-linear", isLeftLinear)
    , ("right-linear", isRightLinear)
    , ("right-reducible", isRightReducible)
    , ("erasing", isErasing)
    , ("creating", isCreating)
    , ("expanding", isExpanding)
    , ("duplicating", isDuplicating)
    , ("collapsing", isCollapsing)
    , ("one-rule", isOneRule)
    , ("srs", isSrs)
    , ("amiguous", isAmiguous)
    , ("orthogonal", isOrthogonal)
    ]


isSrs :: [Rule f v] -> Bool
isSrs = all (R.both isString)
    where
        isString = all (\(_,a) -> 1 == a)  . T.funs . T.withArity

isOneRule :: [Rule f v] -> Bool
isOneRule [_] = True
isOneRule _   = False

isShallow :: [Rule f v] -> Bool
isShallow = all (R.both shallow)
    where
        shallow (Var _) = True
        shallow (Fun _ args) =
            all (\t -> T.isVar t || T.isGround t) args

isRightReducible :: (Eq f, Ord v) => [Rule f v] -> Bool
isRightReducible trs = all (reducible . R.rhs) trs
    where
        reducible = not . null . fullRewrite trs

isFlat :: [Rule f v] -> Bool
isFlat = all $ R.both (\t -> T.isVar t || isConst t)
    where
        isConst (Fun _ []) = True
        isConst _ = False

isAmiguous :: (Ord v, Eq f) => [Rule f v] -> Bool
isAmiguous = not . null . CP.cps'

isOrthogonal :: (Ord v, Eq f) => [Rule f v] -> Bool
isOrthogonal trs = isLeftLinear trs && not (isAmiguous trs)

