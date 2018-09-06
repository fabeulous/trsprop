module Properties
    ( properties
    ) where

import Data.Rewriting.Rules
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
    ]


isShallow :: [Rule f v] -> Bool
isShallow = all (R.both shallow)
    where
        shallow (Var _) = True
        shallow (Fun f args) =
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

