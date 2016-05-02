{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module Trainz.Model (Station, HasStation(..), FromBson(..)) where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens.TH
import Control.Lens.Getter
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Bson as Bson
import Data.Word
import Data.Time.Calendar
import Data.Attoparsec.Text
import Data.Time.Format
import qualified Data.ByteString as BS
import System.IO
import Data.Time.Clock
import Control.Lens.Operators
import Data.Set

class FromBson a where
  fromBson :: Document -> Maybe a


lensAesonOptions :: Int -> Aeson.Options
lensAesonOptions n = defaultOptions {
  Aeson.fieldLabelModifier = drop (n + 1)
  }

data Betriebsstelle =
  Betriebsstelle {
    _bsAbbrev :: Text,
    _bsShortName :: Text,
    _bsCountryCode :: Maybe Text,
    _bsLocationCode :: Maybe Word16,
    _bsValidSince :: Maybe Day
    } deriving (Show, Eq, Typeable, Generic)

data Bahnhof =
  Bahnhof {
    _bfState :: Text,
    _bfManagement:: Text,
    _bfNumber :: Int,
    _bfName :: Text,
    _bfDs100 :: Text,
    _bfCategory :: Int,
    _bfStreet :: Maybe Text,
    _bfZip :: Maybe Int,
    _bfCity :: Maybe Text,
    _bfCompany :: Text,
    _bfTransportSystem :: Text,
    _bfNationalService :: Bool,
    _bfRegionalService :: Bool,
    _bfStation :: Betriebsstelle,
    _bfEva :: Maybe Int
    } deriving (Show, Typeable, Generic)

data Location =
  Location {
    _locationBS :: Betriebsstelle,
    _locationStation :: Maybe Bahnhof
    } deriving (Show, Typeable, Generic)
  

makeClassy ''Betriebsstelle
makeClassy ''Bahnhof

instance Eq Bahnhof where
  a == b =
    ((a ^. bfDs100) == (b ^. bfDs100)) &&
    ((a ^. bfEva) == (b ^. bfEva))

instance Ord Bahnhof where
  compare a b =
    let dsa = a ^. bfDs100
        dsb = b ^. bfDs100
   in compare dsa dsb

instance ToJSON Betriebsstelle where
  toEncoding = genericToEncoding (lensAesonOptions 2)

instance Val Betriebsstelle where
  cast' (Bson.Doc d) = 
    Betriebsstelle
      <$> Bson.lookup "abbrev" d
      <*> Bson.lookup "shortName" d
      <*> Bson.lookup "countryCode" d
      <*> (fmap toLocationCode <$> Bson.lookup "locationCode" d)
      <*> (fmap utctDay <$> Bson.lookup "validSince" d)
      where toLocationCode :: Int -> Word16
            toLocationCode = fromIntegral
  cast' _ = Nothing
  val bs = Doc [
    "abbrev" =: bs ^. bsAbbrev,
    "shortName" =: bs ^. bsShortName,
    "countryCode" =: bs ^. bsCountryCode,
    "locationCode" =: toLocationCode (bs ^. bsLocationCode),
    "validSince" =: toValidSince (bs ^. bsValidSince)
    ]
    where
      toValidSince :: Maybe Day -> Maybe UTCTime
      toValidSince Nothing = Nothing
      toValidSince (Just d) = Just $ UTCTime d 0
      toLocationCode :: Maybe Word16 -> Maybe Int
      toLocationCode = fmap fromIntegral
        

readEvaLines :: FilePath -> IO [(Text,Int)]
readEvaLines fn = (fmap readBsLine . drop 1) <$> readCsvLines fn
  where readBsLine [e, ds100, _, _, _, _, _, _] = (ds100, read . T.unpack $ e)
        readBsLine a = error $ "readEvaLines: expecting 8 fields, got " ++
          (show $ length a)
       
readEvaMap :: FilePath -> IO (Map Text Int)
readEvaMap fn = Map.fromList <$> readEvaLines fn 

readBsLines :: FilePath -> IO [Betriebsstelle]
readBsLines fn = (fmap readBsLine . drop 1) <$> readCsvLines fn
  where readBsLine [ab, sn, cc, lc, vs] =
          let vs' "" = Nothing
              vs' a = pure $ dp a
              lc' "" = Nothing
              lc' a = pure . read . T.unpack $ a
              cc' "" = Nothing
              cc' a = pure a
          in Betriebsstelle ab sn (cc' cc) (lc' lc) (vs' vs)
        readBsLine _ = error "radBsLines: expecting 5 fields"

readBsMap :: FilePath -> IO (Map Text Betriebsstelle)
readBsMap fp = Map.fromList . fmap bsPair <$> readBsLines fp 
  where bsPair bs = (bs ^. bsAbbrev, bs)


readBfLines :: Map Text Betriebsstelle -> Map Text Int -> FilePath -> IO [Bahnhof]
readBfLines bsMap evaMap fn =
  (fmap readBfLine . drop 1) <$> readCsvLines fn
  where readBfLine [st, reg, no, n, ds100, cat, str, zip, city, comp, trans, natsrv, regsrv] =
          let readB "nein" = False
              readB "ja" = True
              readB a = error $ "readB: expecting yes/no, got: " ++ show a
              readZip :: Text -> Maybe Int
              readZip a = read . T.unpack <$> readT a
              readT "" = Nothing
              readT a = pure a
              lookupBs a =
                fromMaybe
                (error $ "unable to lookup Betriebsstelle" ++ show a) .
                Map.lookup a $ bsMap
              lookupEva a = Map.lookup a evaMap
                
          in Bahnhof st reg (read . T.unpack $ no) n ds100 (read . T.unpack $ cat) (readT str) (readZip zip) (readT city) comp trans (readB natsrv) (readB regsrv) (lookupBs ds100) (lookupEva ds100)
        readBfLine a = error $ "readBfLines: expecting 13 fields got " ++
          (show $ a)

readBfSet :: Map Text Betriebsstelle -> Map Text Int -> FilePath -> IO (Set Bahnhof)
readBfSet bsMap evaMap fn = Set.fromList <$> readBfLines bsMap evaMap fn

foo = do
  tbs <- readBsMap "data/DBNetz-Betriebsstellenverzeichnis-Stand2015-05.csv"
  eva <- readEvaMap "data/D_Bahnhof_2016_01_alle.csv"
  tbfr <- readBfSet tbs eva "data/DBRNI-Uebersicht_Bahnhoefe-Stand2016-01.csv"
  tbf <- readBfSet tbs eva "data/DBSuS-Uebersicht_Bahnhoefe-Stand2016-03.csv"
  let bfs = Set.union tbfr tbf
      ds100s = Set.toList $ Set.map (view bfDs100) bfs
      tbs' = delKeys tbs ds100s
  return ()

delKeys :: Ord k => Map k a -> [k] -> Map k a
delKeys m [] = m
delKeys m (k:ks) =
  if Map.size m == 0 then m else delKeys (Map.delete k m) ks

readCsvLines :: FilePath -> IO [[Text]]
readCsvLines fn = withFile fn ReadMode (readCsvLines' mempty)
readCsvLines' xs h = do
  eof <- hIsEOF h
  if eof then return xs
    else do
    fs <- splitHandleLine h
    readCsvLines' (fs : xs) h
  where
    splitHandleLine = fmap (T.split (== ';') . T.decodeLatin1) . BS.hGetLine
    
dp :: Text -> Day
dp a =
  case dps $ T.unpack a of
    (a', _):_ -> a'
    _ -> error $ "unable to parse date from: " ++ T.unpack a
  where dps :: ReadS Day
        dps = readSTime False defaultTimeLocale (iso8601DateFormat Nothing) 


data Station =
  Station {
    _stationId :: Text, 
    _stationName :: Text,
    _railwayStationCode :: Text,
    _spokeStartIds :: [Text],
    _spokeEndIds :: [Text]
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''Station



instance ToJSON Station where
  toEncoding = genericToEncoding (lensAesonOptions 0)

instance FromBson Station where
  fromBson d = do
    props <- Bson.lookup "properties" d
    Station
      <$> Bson.lookup "id" props
      <*> Bson.lookup "geographicalName" props
      <*> Bson.lookup "railwayStationCode" props
      <*> Bson.lookup "spokeStartIds" props
      <*> Bson.lookup "spokeEndIds" props



