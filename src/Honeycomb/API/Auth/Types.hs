{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Honeycomb.API.Auth.Types where

import Control.Exception
import Data.Aeson.TH (defaultOptions, deriveFromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy as L
import Data.Text (Text)
import Honeycomb.Aeson (snakeCaseOptions)

data NameAndSlug = NameAndSlug
  { name :: Text
  , slug :: Text
  }
  deriving stock (Show, Eq)

$(deriveFromJSON snakeCaseOptions ''NameAndSlug)

data ApiKeyAccess = ApiKeyAccess
  { events :: Bool
  , markers :: Bool
  , triggers :: Bool
  , boards :: Bool
  , queries :: Bool
  , columns :: Bool
  , createDatasets :: Bool
  , slos :: Bool
  , recipients :: Bool
  , privateBoards :: Bool
  }
  deriving stock (Show, Eq)

-- XXX: yes, this is intentionally defaultOptions, because for some reason the
-- API is generally snake case but the "createDatasets" is camel case
$(deriveFromJSON defaultOptions ''ApiKeyAccess)

-- | Response to the auth API
--  @
--  {
--    "api_key_access": {
--      "events": true,
--      "markers": true,
--      "triggers": true,
--      "boards": true,
--      "queries": true,
--      "columns": false,
--      "createDatasets": true
--    },
--    "environment": {
--      "name": "Production",
--      "slug": "production"
--    },
--    "team": {
--      "name": "Honeycomb Docs",
--      "slug": "honeycomb-docs"
--    }
--  }
--  @
data Auth = Auth
  { apiKeyAccess :: ApiKeyAccess
  , environment :: NameAndSlug
  , team :: NameAndSlug
  }
  deriving stock (Show, Eq)

$(deriveFromJSON snakeCaseOptions ''Auth)

data FailureResponse
  = FailureCode Int L.ByteString
  | JsonDecodeFailed Text
  deriving stock (Show)
  deriving anyclass (Exception)
