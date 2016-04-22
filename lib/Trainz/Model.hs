{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module Trainz.Model (Station, HasStation(..), FromBson(..)) where

import Control.Lens.TH
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Data.Bson as Bson

lensAesonOptions :: Aeson.Options
lensAesonOptions = defaultOptions {
  Aeson.fieldLabelModifier = drop 1
  }

class FromBson a where
  fromBson :: Document -> Maybe a


data Station =
  Station {
    _stationId :: Text, 
    _stationName :: Text,
    _railwayStationCode :: Text,
    _spokeStartIds :: [Text],
    _spokeEndIds :: [Text]
  } deriving (Show, Typeable, Generic)

makeClassy ''Station



instance ToJSON Station where
  toEncoding = genericToEncoding lensAesonOptions

instance FromBson Station where
  fromBson d = do
    props <- Bson.lookup "properties" d
    Station
      <$> Bson.lookup "id" props
      <*> Bson.lookup "geographicalName" props
      <*> Bson.lookup "railwayStationCode" props
      <*> Bson.lookup "spokeStartIds" props
      <*> Bson.lookup "spokeEndIds" props
