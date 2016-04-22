{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Trainz.Model
import Trainz.Database
import Trainz.Api
import Data.Text (Text)
import Data.Word (Word32)
import Servant
import Database.MongoDB as MongoDB
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger

data TrainzConfig =
  TrainzConfig {
    trainzProduction :: Bool,
    trainzPort :: Int,
    trainzMongoDB :: Database,
    trainzMongoPipe :: Pipe
    }

--
-- Handlers
--
stationsHandler ::
  TrainzConfig ->
  Maybe Text -> Maybe Word32 -> Maybe Word32 -> Handler [Station]
stationsHandler st t l o =
  access (trainzMongoPipe st) ReadStaleOk (trainzMongoDB st) $
    stationsSource t l o $$ CL.consume

stationHandler :: TrainzConfig -> Text -> Handler Station
stationHandler st i = do
  r <- access (trainzMongoPipe st) ReadStaleOk (trainzMongoDB st) $ loadStation i
  case r of
    Nothing -> throwError $ err404 { errBody = "error: unable to lookup station " }
    Just r' -> return r'

stationsNearHandler ::
  TrainzConfig ->
  Maybe Double -> Maybe Double -> Maybe Word32 -> Maybe Word32 -> Maybe Word32 -> Handler [Station]
stationsNearHandler st lat' lon' d' l o = do
  lat <- maybe (throwError $ err412 {
                   errBody = "error: lat parameter missing"}) return lat'
  lon <- maybe (throwError $ err412 {
                   errBody = "error: lon parameter missing"}) return lon'
  d   <- maybe (throwError $ err412 {
                   errBody = "error: d parameter missing"}) return d'
  
  access (trainzMongoPipe st) ReadStaleOk (trainzMongoDB st) $
    stationsNearSource lat lon d l o $$ CL.consume
  
--
-- The application
--  
trainzApplication :: TrainzConfig -> Application
trainzApplication st =
  let logger = if trainzProduction st
               then logStdout else logStdoutDev
  in logger . gzip def . serve trainzAPI . trainzServer $ st
  where trainzAPI :: Proxy TrainzAPI
        trainzAPI = Proxy
        trainzServer :: TrainzConfig -> Server TrainzAPI
        trainzServer st =
          stationsHandler st :<|> stationHandler st :<|> stationsNearHandler st

trainz :: TrainzConfig -> IO ()
trainz st = run (trainzPort st) . trainzApplication $ st

mkTrainsState :: Bool -> Int -> Host -> Database -> IO TrainzConfig
mkTrainsState prod p h d = TrainzConfig prod p d <$> MongoDB.connect h 

main :: IO ()
main = mkTrainsState False 3000 (host "localhost") "db_inspire" >>= trainz
