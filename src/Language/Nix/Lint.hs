module Language.Nix.Lint where

import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy.IO          as TIO
import           Language.Nix.Lint.Checks
import           Language.Nix.Lint.Internal
import qualified Nix.Expr                   as Nix

checkWithAll :: Nix.NExpr -> [Check] -> Results
checkWithAll expr = (`go` Map.empty)
  where go :: [Check] -> Map ID Result -> Results
        go []     rs = Rs rs
        go (c:cs) rs = go cs (case checkFunc c expr of
                                R [] -> rs
                                r    -> Map.insert (checkID c) r rs)

checkWithCfg :: Config -> Nix.NExpr -> [Check] -> Results
checkWithCfg cfg expr = checkWithAll expr . disable cfg

lintWithAll :: [Check] -> Text -> Text
lintWithAll checks = resultsToText checks . (`checkWithAll` checks) . parseExpr

lintWithCfg :: Config -> [Check] -> Text -> Text
lintWithCfg cfg = lintWithAll . disable cfg

nixLintMain :: IO ()
nixLintMain = do
  cfg <- readConfig
  TIO.interact (lintWithCfg cfg defaultChecks)
