module Properties
    ( properties
    ) where

import Data.Rewriting.Rules
import qualified Data.Rewriting.Rules as Rules (isGround, isLeftGround, isRightGround
                                               , isLinear , isRightLinear , isLeftLinear
                                               )
import Data.Rewriting.Rule (Rule)
import qualified Data.Rewriting.Rule as R
import Data.Rewriting.Term (Term (..))
import qualified Data.Rewriting.Term as T

properties :: [(String, [Rule String String] -> Bool)]
properties =
    [ ("shallow", isShallow)
    , ("ground", isGround)
    , ("right-ground", isRightGround)
    , ("linear", isLinear)
    , ("left-linear", isLeftLinear)
    , ("right-linear", isRightLinear)
    , ("right-reducible", isRightReducible)
    , ("flat", isFlat)
    ]


isShallow :: [Rule f v] -> Bool
isShallow = all (R.both shallow)
    where
        shallow (Var _) = True
        shallow (Fun f args) =
            all (\t -> T.isVar t || T.isGround t) args

isRightReducible :: (Eq f, Ord v) => [Rule f v] -> Bool
isRightReducible trs = all (reducable . R.rhs) trs
    where
        reducable = not . null . fullRewrite trs

isFlat :: [Rule f v] -> Bool
isFlat = all $ R.both (\t -> T.isVar t || isConst t)
    where
        isConst (Fun _ []) = True
        isConst _ = False

