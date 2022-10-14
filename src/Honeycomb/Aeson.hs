module Honeycomb.Aeson (snakeCaseOptions) where
import Data.Aeson

snakeCaseOptions :: Options
snakeCaseOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    , constructorTagModifier = camelTo2 '_'
    }


