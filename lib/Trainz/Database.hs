{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Trainz.Database
       ( stationsSource, loadStation, stationsNearSource
       ) where

import Database.MongoDB as MongoDB
import Trainz.Model
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Data.Text(Text)
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Word
import Data.Maybe(fromMaybe)


railwayStationNodesCol :: Collection
railwayStationNodesCol = "railwayStationNodes"


stationsSource ::
  (MonadIO m, MonadBaseControl IO m) =>
  Maybe Text -> Maybe Word32 -> Maybe Word32 -> Source (Action m) Station
stationsSource = textSearchSource railwayStationNodesCol

loadStation :: MonadIO m => Text -> Action m (Maybe Station)
loadStation t = do
  r <- findOne $ select [ "properties.id" =: t ] railwayStationNodesCol
  return $ case r of
    Nothing -> Nothing
    Just r' -> fromBson r'


stationsNearSource ::
  (MonadIO m, MonadBaseControl IO m) =>
  Double -> Double -> Word32 -> Maybe Word32 -> Maybe Word32 ->
  Source (Action m) Station
stationsNearSource lat lon d limit offset =
  let sel = mkNearSelector lat lon d
      src = querySource q'
      q = select sel railwayStationNodesCol
      q' = q { MongoDB.limit = limit', MongoDB.skip = offset' }
      limit' = min 100 $ fromMaybe 10 limit
      offset' = fromMaybe 0 offset
  in mapOutputMaybe fromBson src


--
-- Helpers
--

cursorSource ::
  (MonadIO m, MonadBaseControl IO m) => Cursor -> Source (Action m) Document
cursorSource c = 
  lift (next c) >>=
    maybe (return ()) (\v' -> yield v' >> cursorSource c)
    
querySource ::
    (MonadIO m, MonadBaseControl IO m) =>
    Query -> Source (Action m) Document
querySource q = lift (find q) >>= cursorSource

textSearchSource ::
  (MonadIO m, MonadBaseControl IO m, FromBson a) =>
  Collection -> Maybe Text -> Maybe Word32 -> Maybe Word32 -> Source (Action m) a
textSearchSource c qname limit offset =
  let s = case qname of
        Nothing -> []
        Just qname' -> [ "$text" =: [ "$search" =: qname' ] ]
      q = select s c
      limit' = min 100 $ fromMaybe 10 limit
      offset' = fromMaybe 0 offset
      q' = q { MongoDB.limit = limit', MongoDB.skip = offset' }
  in mapOutputMaybe fromBson . querySource $ q'


mkNearSelector :: Double -> Double -> Word32 -> Selector
mkNearSelector lat lon d =
  [ "geometry" =:
    [ "$near" =:
      [ "$geometry" =:
        [ "type" =: ("Point" :: String)
        , "coordinates" =: [ lat, lon ]
        ]
      , "$maxDistance" =: ((fromInteger . toInteger $ d) :: Int)
      ]
    ]
  ]
