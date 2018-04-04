{-# LANGUAGE LambdaCase, OverloadedStrings, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Language.Nix.Lint.Checks where

import Data.Fix                   (cata)
import Data.Foldable              (fold)
import Data.Text.Lazy             (Text)
import Language.Nix.Lint.Internal
import Nix.Expr

defaultChecks :: [Check]
defaultChecks = [checkNoLet]

checkNoLet = Check {
    checkID   = "nolet"
  , checkDesc = "Prefers 'with {...}; ...' to 'let ... in ...'"
  , checkFunc = recurseKeepOne (\case
                                 NLet _ _ -> ["'let' is redundant, use 'with'"]
                                 x        -> fold x)
  }

-- Helpers

-- Run the given function over all sub-expressions and accumulate all of the
-- generated messages (if any).
recurseThroughAll :: (NExprF _ -> [Text]) -> NExpr -> Result
recurseThroughAll f = R . cata f

-- Run the given function over all sub-expressions, but return at most one
-- message. This is useful if all messages are the same (e.g. "Don't use
-- hyphens") so reporting it once is enough.
recurseKeepOne :: (NExprF _ -> [Text]) -> NExpr -> Result
recurseKeepOne f = R . take 1 . cata f
