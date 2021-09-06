-- SPDX-FileCopyrightText: Maximilian Huber
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Opossum.Opossum
  ( Opossum_Resources (..), countFiles
  , fpToResources, fpsToResources
  , Opossum_FrequentLicense (..)
  , Opossum_Coordinates (..), purlToCoordinates
  , Opossum_ExternalAttribution (..), Opossum_ExternalAttribution_Source (..)
  , Opossum (..)
  , writeOpossumStats
  ) where

import GHC.Generics
import Data.UUID (UUID)
import System.Random (randomIO)
import System.IO (Handle, hPutStrLn, hClose, stdout)
import PURL.PURL
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import qualified Data.HashMap.Strict as HM
import qualified System.Process as P
import qualified Data.ByteString.Lazy as B
import qualified System.FilePath as FP

objectNoNulls :: [A.Pair] -> A.Value
objectNoNulls = let
  dropNulls []              = []
  dropNulls ((_,A.Null):ps) = dropNulls ps
  dropNulls (p:ps)          = p : (dropNulls ps)
  in A.object . dropNulls

data Opossum_Resources
  = Opossum_Resources 
  { _dirs :: Map.Map FilePath Opossum_Resources
  , _files :: Set.Set FilePath
  } deriving (Show, Generic, Eq)
instance A.ToJSON Opossum_Resources where
    toJSON (Opossum_Resources dirs files) = let
        pairsFromDirs = 
            map (\(fragment, resources) -> ((T.pack fragment) A..= (A.toJSON resources)))
            (Map.toList dirs) 
        pairsFromFiles = map (\fragment ->  (T.pack fragment) A..= (1::Int)) $ Set.toList files
      in A.object (pairsFromFiles ++ pairsFromDirs)
instance A.FromJSON Opossum_Resources where
  parseJSON = A.withObject "Opossum_Resources" $ \v -> let
      isObject :: A.Value -> Bool
      isObject (A.Object _) = True
      isObject _            = False
      getFiles :: [(FilePath, A.Value)] -> Set.Set FilePath
      getFiles = Set.fromList . map (\(fp,_) -> fp) . (filter (\(_,v') -> not (isObject v')))
      getDirs :: [(FilePath, A.Value)] -> A.Parser (Map.Map FilePath Opossum_Resources)
      getDirs list = do
        parsedList <- mapM (\(fp,o) -> do 
          rs <- A.parseJSON o :: A.Parser Opossum_Resources
          return (fp,rs)) (filter (\(_,v') -> isObject v') list)
        return (Map.fromList parsedList)
    in do
      let vList = map (\(t,v') -> (T.unpack t, v')) $ HM.toList v
      dirs <- getDirs vList
      return (Opossum_Resources dirs (getFiles vList))
instance Semigroup Opossum_Resources where
  (Opossum_Resources dirs1 files1) <> (Opossum_Resources dirs2 files2) = let
    dirs = (Map.unionWith (<>) dirs1 dirs2)
    dirNames = Map.keys dirs
    files = Set.filter (\f -> not $ f `elem` dirNames) $ files1 <> files2
    in Opossum_Resources dirs files
instance Monoid Opossum_Resources where
  mempty = Opossum_Resources Map.empty Set.empty
fpToResources :: Bool -> FilePath -> Opossum_Resources
fpToResources isFile = let
    fpToResources' :: [FilePath] -> Opossum_Resources
    fpToResources' (f : [])  = if isFile
                               then Opossum_Resources (Map.empty) (Set.singleton f)
                               else Opossum_Resources (Map.singleton f mempty) Set.empty
    fpToResources' ("/": fs) = fpToResources' fs
    fpToResources' (f : fs)  = Opossum_Resources (Map.singleton f (fpToResources' fs)) Set.empty
  in fpToResources' . (map FP.dropTrailingPathSeparator) . FP.splitPath
fpsToResources :: [FilePath] -> Opossum_Resources
fpsToResources = mconcat . map (fpToResources True)
countFiles :: Opossum_Resources -> Int
countFiles (Opossum_Resources dirs files) = length files + ((sum . map countFiles . Map.elems) dirs)

data Opossum_ExternalAttribution_Source
  = Opossum_ExternalAttribution_Source String Double
  deriving (Show, Generic, Eq)
instance A.ToJSON Opossum_ExternalAttribution_Source where
    toJSON (Opossum_ExternalAttribution_Source source documentConfidence) =
        A.object [ "name" A..= (T.pack source)
                 , "documentConfidence" A..= documentConfidence
                 ]
instance A.FromJSON Opossum_ExternalAttribution_Source where
    parseJSON = A.withObject "Opossum_ExternalAttribution_Source" $ \v -> do
        Opossum_ExternalAttribution_Source <$> v A..: "name"
                                       <*> v A..: "documentConfidence"

data Opossum_Coordinates
  = Opossum_Coordinates
  { _packageName :: Maybe T.Text
  , _packageNamespace :: Maybe T.Text
  , _packageType :: Maybe T.Text
  , _packageVersion :: Maybe T.Text
  } deriving (Show, Generic, Eq)
opoossumCoordinatesPreObjectList :: Opossum_Coordinates -> [A.Pair]
opoossumCoordinatesPreObjectList 
    (Opossum_Coordinates 
        packageType
        packageNamespace
        packageName
        packageVersion) = [ "packageType" A..= packageType
                          , "packageNamespace" A..= packageNamespace
                          , "packageName" A..= packageName
                          , "packageVersion" A..= packageVersion
                          ]
instance A.ToJSON Opossum_Coordinates where
    toJSON = objectNoNulls . opoossumCoordinatesPreObjectList
instance A.FromJSON Opossum_Coordinates where
    parseJSON = A.withObject "Opossum_Coordinates" $ \v -> do
        packageType <- v A..:? "packageType"
        packageNamespace <- v A..:? "packageNamespace"
        packageName <- v A..:? "packageName"
        packageVersion <- v A..:? "packageVersion"
        return $ Opossum_Coordinates packageType packageNamespace packageName packageVersion
purlToCoordinates :: PURL -> Opossum_Coordinates
purlToCoordinates (PURL { _PURL_type = type_
                        , _PURL_namespace = namespace
                        , _PURL_name = name
                        , _PURL_version = version
                        }) = Opossum_Coordinates (fmap (T.pack . show) type_)
                                                 (fmap T.pack namespace)
                                                 (Just $ T.pack name)
                                                 (fmap T.pack version)

data Opossum_ExternalAttribution
  = Opossum_ExternalAttribution
  { _source :: Opossum_ExternalAttribution_Source
  , _attributionConfidence :: Double
  , _comment :: Maybe T.Text
  , _originId :: Maybe UUID
  , _coordinates :: Opossum_Coordinates
  , _copyright :: Maybe T.Text
  , _licenseName :: Maybe T.Text
  , _licenseText :: Maybe T.Text
  , _preselected :: Bool
  } deriving (Show, Generic, Eq)
instance A.ToJSON Opossum_ExternalAttribution where
    toJSON (Opossum_ExternalAttribution 
        source
        attributionConfidence
        comment
        originId
        coordinates
        copyright
        licenseName
        licenseText
        preselected) = let
            maybePreselected = if preselected
              then Just True
              else Nothing
          in objectNoNulls ([ "source" A..= source
                            , "attributionConfidence" A..= attributionConfidence
                            , "comment" A..= comment
                            , "copyright" A..= copyright
                            , "licenseName" A..= licenseName
                            , "licenseText" A..= licenseText 
                            , "originId" A..= originId
                            , "preSelected" A..= maybePreselected
                            ] ++ (opoossumCoordinatesPreObjectList coordinates))
instance A.FromJSON Opossum_ExternalAttribution where
  parseJSON = A.withObject "Opossum_ExternalAttribution" $ \v -> do
    source <- v A..: "source"
    attributionConfidence <- fmap (100 `Maybe.fromMaybe`) (v A..:? "attributionConfidence")
    comment <- v A..:? "comment"
    originId <- fmap (fmap read . (\case
      Just "" -> Nothing
      x       -> x)) (v A..:? "originId" :: A.Parser (Maybe String))
    coordinates <- A.parseJSON (A.Object v) :: A.Parser Opossum_Coordinates
    copyright <- v A..:? "copyright"
    licenseName <- fmap (\case
      Just "" -> Nothing
      l -> l) (v A..:? "licenseName")
    licenseText <- fmap (\case
      Just "" -> Nothing
      l -> l) (v A..:? "licenseText")
    preselected <- fmap (False `Maybe.fromMaybe`) (v A..:? "preSelected")

    return (Opossum_ExternalAttribution source attributionConfidence comment originId coordinates copyright licenseName licenseText preselected)

data Opossum_FrequentLicense
  = Opossum_FrequentLicense
  { shortName :: T.Text
  , fullName :: T.Text
  , defaultText :: T.Text
  } deriving (Eq, Show, Generic)
instance A.ToJSON Opossum_FrequentLicense where
  toJSON (Opossum_FrequentLicense sn fn dt) = A.object [ "shortName" A..= sn
                                                   , "fullName" A..= fn
                                                   , "defaultText"  A..= dt
                                                   ]
instance A.FromJSON Opossum_FrequentLicense where
  parseJSON = A.withObject "Opossum_FrequentLicense" $ \v -> do
    Opossum_FrequentLicense <$> v A..: "shortName"
                        <*> v A..: "fullName"
                        <*> v A..: "defaultText"

data Opossum
  = Opossum
  { _metadata :: Maybe A.Value
  , _resources :: Opossum_Resources
  , _externalAttributions :: Map.Map UUID Opossum_ExternalAttribution
  , _resourcesToAttributions :: Map.Map FilePath [UUID]
  , _frequentLicenses :: [Opossum_FrequentLicense]
  } deriving (Show, Generic)
instance A.ToJSON Opossum where
    toJSON (Opossum
        metadata
        resources
        externalAttributions
        resourcesToAttributions
        frequentLicenses) = objectNoNulls
          [ "metadata" A..= metadata
          , "resources" A..= resources
          , "externalAttributions" A..= externalAttributions
          , "resourcesToAttributions" A..= resourcesToAttributions
          , "frequentLicenses" A..= frequentLicenses
          ]
instance A.FromJSON Opossum where
  parseJSON = A.withObject "Opossum" $ \v -> do
    let
      resourcesParser = fmap (mempty `Maybe.fromMaybe`) $ v A..:? "resources"
      externalAttributionsParser = fmap (mempty `Maybe.fromMaybe`) $ v A..:? "externalAttributions"
      resourcesToAttributionsParser = fmap (mempty `Maybe.fromMaybe`) $ v A..:? "resourcesToAttributions"
    Opossum <$> v A..:? "metadata"
        <*> resourcesParser
        <*> externalAttributionsParser
        <*> resourcesToAttributionsParser
        <*> (fmap (\case 
            Just fls -> fls
            Nothing -> []) (v A..:? "frequentLicenses"))
instance Semigroup Opossum where
    opossum1 <> opossum2 = let
          mergedResources = _resources opossum1 <> _resources opossum2 
          mergedExternalAttributions = Map.union (_externalAttributions opossum1) (_externalAttributions opossum2)
          mergedResourcesToAttributions = Map.unionWith (++) (_resourcesToAttributions opossum1) (_resourcesToAttributions opossum2) -- TODO: nub
          mergedFrequentLicenses = List.nub (_frequentLicenses opossum1 ++ _frequentLicenses opossum2)
        in Opossum (_metadata opossum1) 
               mergedResources
               mergedExternalAttributions
               mergedResourcesToAttributions
               mergedFrequentLicenses
instance Monoid Opossum where
    mempty = Opossum Nothing mempty Map.empty Map.empty []

writeOpossumStats :: Opossum -> IO ()
writeOpossumStats (Opossum { _metadata = m
                   , _resources = rs
                   , _externalAttributions = eas
                   , _resourcesToAttributions = rtas
                   , _frequentLicenses = fls
                   }) = do
                     putStrLn ("metadata: " ++ show m)
                     putStrLn ("resources: #files=" ++ (show (countFiles rs)))
                     putStrLn ("externalAttributions: #=" ++ (show (length eas)))
                     putStrLn ("resourcesToAttributions: #=" ++ (show (length rtas)))
                     putStrLn ("frequentLicenses: #=" ++ (show (length fls)))
