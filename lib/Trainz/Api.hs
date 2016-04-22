{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}

module Trainz.Api where

import Servant.API
import Trainz.Model
import Data.Text (Text)
import Data.Word

type StationAPI =
  "stations" :>
  QueryParam "matchStationName" Text :>
  QueryParam "limit" Word32 :>
  QueryParam "offset" Word32 :>
  Get '[JSON] [Station]
  :<|>
  "station" :> Capture "stationId" Text :> Get '[JSON] Station

type TrainzAPI = StationAPI
