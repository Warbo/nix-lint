module Language.Nix.Lint where

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy         as Text
import qualified Nix.Expr               as Nix
import qualified Nix.Parser             as Parser
import qualified System.Environment     as Env

newtype ID = ID Text

newtype Result = R [Text]

newtype Results = Map ID Result

newtype Check = Check {
    checkID   :: ID
  , checkName :: Text
  , checkFunc :: Nix.Expr -> Maybe Result
  }

newtype Config = Cfg { cfgDisabled :: [ID] }

defaultConfig = Cfg []

defaultChecks :: [Check]
defaultChecks = []

checkWithAll :: Nix.Expr -> [Check] -> Results
checkWithAll expr = (`go` Map.empty)
  where go []     rs = rs
        go (c:cs) rs = go cs (case checkFunc c expr of
                                Nothing -> rs
                                Just r  -> Map.insert (checkId c) r rs)

checkWithCfg :: Config -> Expr -> [Check] -> Results
checkWithCfg cfg expr = checkWithAll expr . disable cfg

disable :: Config -> [Check] -> [Check]
disable cfg = filter ((`notElem` cfgDisabled cfg) . checkID)

readConfig :: IO Config
readConfig = Cfg . maybe [] read <$> Env.lookupEnv "NIX_LINT_DISABLE"

err :: Aeson.Value -> a
err = error . show . encode

lookup' :: k -> Map k v -> v
lookup' k m = Map.findWithDefault (err msg) k v
  where msg = object ["error" .= "Missing name for ID",
                      "id"    .= id                   ,
                      "names" .= names                ]

resultsToText :: [Check] -> Results -> Text
resultsToText checks = Map.foldlWithKey' go Text.empty
  where go acc         = Text.append acc . Aeson.encode . mkMap
        mkMap id (R r) = object ["id"          .= id              ,
                                 "name"        .= lookup' id names,
                                 "suggestions" .= r               ]
        names          = Map.fromList (map getName checks)
        getName c      = (checkID c, checkName c)

parseExpr = handle . Nix.parseNix
  where handle (Left  e) = err (object ["error"   .= "Parse failure",
                                        "details" .= e              ])
        handle (Right x) = x

lintWithAll :: [Check] -> Text -> Text
lintWithAll checks = resultsToText . (`checkWithAll` checks) . parseExpr

lintWithCfg :: Config -> [Check] -> Text -> Text
lintWithCfg = lintWithAll . disable

nixLintMain = do
  cfg <- readConfig
  interact (lintWithCfg cfg defaultChecks)
