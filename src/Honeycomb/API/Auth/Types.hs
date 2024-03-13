{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Honeycomb.API.Auth.Types where

import Control.Exception
import Data.Aeson
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

instance FromJSON ApiKeyAccess where
  parseJSON (Object v) = do
    events <- v .:? "events" .!= False
    markers <- v .:? "markers" .!= False
    triggers <- v .:? "triggers" .!= False
    boards <- v .:? "boards" .!= False
    queries <- v .:? "queries" .!= False
    columns <- v .:? "columns" .!= False
    createDatasets <- v .:? "createDatasets" .!= False
    slos <- v .:? "slos" .!= False
    recipients <- v .:? "recipients" .!= False
    privateBoards <- v .:? "privateBoards" .!= False
    pure ApiKeyAccess {..}

-- | Responses to the auth API
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
--
--
-- {
--   "api_key_access": {
--     "createDatasets": true
--   },
--   "environment": {
--     "name": "dev",
--     "slug": "dev"
--   },
--   "team": {
--     "name": "Mercury",
--     "slug": "mercury"
--   },
--   "type": "ingest"
-- }
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
