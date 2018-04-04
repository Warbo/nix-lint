module Language.Nix.Lint.Tests where

import qualified Data.Map as Map
import Language.Nix.Lint
import Language.Nix.Lint.Checks
import Nix.Expr
import Test.QuickCheck

prop_letsAreSpotted = checkResult genLet checkNoLet test
  where test r = case Map.toList r of
          [(id, _)] -> id === checkID checkNoLet
          _         -> counterexample (show r) (pure False)

checkResult gen check test = forAll gen ((\(Rs r) -> test r) . check)

genLet = mkLet <$> arbitrary <*> arbitrary

-- Wrap the given expression inside a bunch of others
genWrapperAround x = immediate <|> genWrapperAround immediate
  where immediate = oneof [
            do x'                     <- x
               (pre, post, val, bind) <- arbitrary
               return (if bind
                          then mkLet (pre ++ [x] ++ post) val
                          else mkLet (pre ++        post) x)
          ]
