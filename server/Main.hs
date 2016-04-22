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

data TrainzState =
  TrainzState {
    trainzProduction :: Bool,
    trainzPort :: Int,
    trainzMongoDB :: Database,
    trainzMongoPipe :: Pipe
    }
  
stationsHandler ::
  TrainzState ->
  Maybe Text -> Maybe Word32 -> Maybe Word32 -> Handler [Station]
stationsHandler st t l o =
  access (trainzMongoPipe st) ReadStaleOk (trainzMongoDB st) $
    searchStationsSource t l o $$ CL.consume

stationHandler :: TrainzState -> Text -> Handler Station
stationHandler st i = do
  r <- access (trainzMongoPipe st) ReadStaleOk (trainzMongoDB st) $ loadStation i
  case r of
    Nothing -> throwError $ err404 { errBody = "unable to lookup station " }
    Just r' -> return r'
    
trainzApplication :: TrainzState -> Application
trainzApplication st =
  let logger = if trainzProduction st
               then logStdout else logStdoutDev
  in logger . gzip def . serve trainzAPI . trainzServer $ st
  where trainzAPI :: Proxy TrainzAPI
        trainzAPI = Proxy
        trainzServer :: TrainzState -> Server TrainzAPI
        trainzServer st = stationsHandler st :<|> stationHandler st

trainz :: TrainzState -> IO ()
trainz st = run (trainzPort st) . trainzApplication $ st

mkTrainsState :: Bool -> Int -> Host -> Database -> IO TrainzState
mkTrainsState prod p h d = TrainzState prod p d <$> MongoDB.connect h 

main :: IO ()
main = mkTrainsState False 3000 (host "localhost") "db_inspire" >>= trainz
