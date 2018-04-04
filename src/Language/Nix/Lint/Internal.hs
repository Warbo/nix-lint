{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings,
             PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Language.Nix.Lint.Internal where

import           Data.Aeson                    ((.=))
import qualified Data.Aeson                 as Aeson
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.String                as String
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy             as Text
import qualified Data.Text.Lazy.Encoding    as TE
import           GHC.Generics                  (Generic)
import qualified Nix.Expr                   as Nix
import qualified Nix.Parser                 as Parser
import qualified System.Environment         as Env

newtype ID = ID Text deriving (Eq, Generic, Ord, Read)
instance Aeson.ToJSON    ID
instance Aeson.ToJSONKey ID

instance String.IsString ID where
  fromString = ID . String.fromString

newtype Result = R [Text]

newtype Results = Rs (Map ID Result)

data Check = Check {
    checkID   :: ID
  , checkDesc :: Text
  , checkFunc :: Nix.NExpr -> Result
  }

newtype Config = Cfg { cfgDisabled :: [ID] }

defaultConfig = Cfg []

readConfig :: IO Config
readConfig = Cfg . maybe [] read <$> Env.lookupEnv "NIX_LINT_DISABLE"

disable :: Config -> [Check] -> [Check]
disable cfg = filter ((`notElem` cfgDisabled cfg) . checkID)

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
        go acc id r = case r of
          R [] -> acc
          R ss -> Text.append acc . TE.decodeUtf8 . Aeson.encode . mkMap id $ ss

        mkMap :: ID -> [Text] -> Aeson.Value
        mkMap id ss = Aeson.object ["id"          .= id              ,
                                    "desc"        .= lookup' id descs,
                                    "suggestions" .= ss              ]

        descs          = Map.fromList (map getDesc checks)
        getDesc c      = (checkID c, checkDesc c)

parseExpr :: Text -> Nix.NExpr
parseExpr = handle . Parser.parseNixText . Text.toStrict
  where handle = \case
          Parser.Failure e -> err (Aeson.object [
                                    "error"   .= ("Parse failure" :: String),
                                    "details" .= show e
                                  ])
          Parser.Success x -> x
