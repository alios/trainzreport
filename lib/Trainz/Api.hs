{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}

module Trainz.Api where

import Servant.API
import Trainz.Model
import Data.Text (Text)
import Data.Word


type StationsApi = 
  "stations" :>
  QueryParam "matchStationName" Text :>
  QueryParam "limit" Word32 :>
  QueryParam "offset" Word32 :>
  Get '[JSON] [Station]

type StationApi = "station" :> Capture "stationId" Text :> Get '[JSON] Station

type StationsNearApi =
  "stationsNear" :>
  Capture "lat" Double :>
  Capture "lon" Double :>
  Capture "maxDistance" Word32 :>
  QueryParam "limit" Word32 :>
  QueryParam "offset" Word32 :>
  Get '[JSON] [Station]
  
type FullStationAPI =
  StationsApi :<|>
  StationApi :<|>
  StationsNearApi

type TrainzAPI = FullStationAPI
