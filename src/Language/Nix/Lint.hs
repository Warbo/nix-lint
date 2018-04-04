{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings,
             PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Language.Nix.Lint where

import           Data.Aeson                 ((.=))
import qualified Data.Aeson              as Aeson
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict         as Map
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO       as TIO
import           GHC.Generics               (Generic)
import qualified Nix.Expr                as Nix
import qualified Nix.Parser              as Parser
import qualified System.Environment      as Env

newtype ID = ID Text deriving (Eq, Generic, Ord, Read)
instance Aeson.ToJSON    ID
instance Aeson.ToJSONKey ID

newtype Result = R [Text]

newtype Results = Rs (Map ID Result)

data Check = Check {
    checkID   :: ID
  , checkName :: Text
  , checkFunc :: Nix.NExpr -> Maybe Result
  }

newtype Config = Cfg { cfgDisabled :: [ID] }

defaultConfig = Cfg []

defaultChecks :: [Check]
defaultChecks = []

checkWithAll :: Nix.NExpr -> [Check] -> Results
checkWithAll expr = (`go` Map.empty)
  where go []     rs = Rs rs
        go (c:cs) rs = go cs (case checkFunc c expr of
                                Nothing -> rs
                                Just r  -> Map.insert (checkID c) r rs)

checkWithCfg :: Config -> Nix.NExpr -> [Check] -> Results
checkWithCfg cfg expr = checkWithAll expr . disable cfg

disable :: Config -> [Check] -> [Check]
disable cfg = filter ((`notElem` cfgDisabled cfg) . checkID)

readConfig :: IO Config
readConfig = Cfg . maybe [] read <$> Env.lookupEnv "NIX_LINT_DISABLE"

err :: Aeson.Value -> a
err = error . show . Aeson.encode

lookup' :: _ => k -> Map k v -> v
lookup' k m = Map.findWithDefault (err msg) k m
  where msg = Aeson.object ["error" .= ("Missing key in map" :: String),
                            "key"   .= k                   ,
                            "map"   .= m                   ]

resultsToText :: [Check] -> Results -> Text
resultsToText checks (Rs rs) = Map.foldlWithKey' go Text.empty rs
  where go :: Text -> ID -> Result -> Text
        go acc id = Text.append acc . TE.decodeUtf8 . Aeson.encode . mkMap id

        mkMap :: ID -> Result -> Aeson.Value
        mkMap id (R r) = Aeson.object ["id"          .= id              ,
                                       "name"        .= lookup' id names,
                                       "suggestions" .= r               ]
        names          = Map.fromList (map getName checks)
        getName c      = (checkID c, checkName c)

parseExpr :: Text -> Nix.NExpr
parseExpr = handle . Parser.parseNixText . Text.toStrict
  where handle = \case
          Parser.Failure e -> err (Aeson.object [
                                    "error"   .= ("Parse failure" :: String),
                                    "details" .= show e
                                  ])
          Parser.Success x -> x

lintWithAll :: [Check] -> Text -> Text
lintWithAll checks = resultsToText checks . (`checkWithAll` checks) . parseExpr

lintWithCfg :: Config -> [Check] -> Text -> Text
lintWithCfg cfg = lintWithAll . disable cfg

nixLintMain :: IO ()
nixLintMain = do
  cfg <- readConfig
  TIO.interact (lintWithCfg cfg defaultChecks)
