{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Trainz.Database
       ( searchStationsSource, loadStation
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


searchStationsSource ::
  (MonadIO m, MonadBaseControl IO m) =>
  Maybe Text -> Maybe Word32 -> Maybe Word32 -> Source (Action m) Station
searchStationsSource = textSearchSource railwayStationNodesCol

loadStation :: MonadIO m => Text -> Action m (Maybe Station)
loadStation t = do
  r <- findOne $ select [ "properties.id" =: t ] railwayStationNodesCol
  return $ case r of
    Nothing -> Nothing
    Just r' -> fromBson r'


test t l o =
  let run = searchStationsSource (pure . T.pack $ t) l o $$ CL.consume
  in do
    pipe <- MongoDB.connect (host "127.0.0.1")
    r <- access pipe master "db_inspire" run
    close pipe
    print $ r
    
