module Properties
    ( isShallow
    , isRightReducible
    , isFlat
    , module Data.Rewriting.Rules
    ) where

import Data.Rewriting.Rules
import Data.Rewriting.Rule as R
import Data.Rewriting.Term as T

isShallow :: [Rule f v] -> Bool
isShallow = all (both shallow)
    where
        shallow (Var _) = True
        shallow (Fun f args) =
            all (\t -> T.isVar t || T.isGround t) args

isRightReducible :: (Eq f, Ord v) => [Rule f v] -> Bool
isRightReducible trs = all (reducable . rhs) trs
    where
        reducable = not . null . fullRewrite trs

isFlat :: [Rule f v] -> Bool
isFlat = all $ both (\t -> T.isVar t || isConst t)
    where
        isConst (Fun _ []) = True
        isConst _ = False

