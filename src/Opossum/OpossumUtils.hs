-- SPDX-FileCopyrightText: Maximilian Huber
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Opossum.OpossumUtils
  ( clusterifyOpossum 
  , unDot
  , dropDir
  , computeMergedOpossum
  , mergifyEA
  ) where

import Opossum.Opossum

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Codec.Compression.GZip as GZip
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Data.ByteString.Lazy as B
import Data.UUID (UUID)
import System.Random (randomIO)

mergifyCopyright :: Maybe Text -> Maybe Text -> Maybe Text
mergifyCopyright left Nothing = left
mergifyCopyright Nothing right = right
mergifyCopyright (Just c) (Just c') = Just . T.unlines . List.nub $ (T.lines c) ++ (T.lines c')

cleanupLicense :: Maybe Text -> Maybe Text
cleanupLicense Nothing  = Nothing
cleanupLicense (Just "") = Nothing
cleanupLicense (Just t) = case T.stripPrefix ", " t of
    Nothing -> Just t
    t'      -> t'

mergifyEA :: Opossum_ExternalAttribution -> Opossum_ExternalAttribution -> Maybe Opossum_ExternalAttribution
mergifyEA
  left@(Opossum_ExternalAttribution
  { _source = Opossum_ExternalAttribution_Source source _
  , _attributionConfidence = attributionConfidence
  , _comment = comment
  , _originId = originId
  , _coordinates = coordinates
  , _copyright = copyright
  , _licenseName = licenseName
  , _licenseText = licenseText
  , _preselected = preselected
  }) 
  (Opossum_ExternalAttribution
  { _source = Opossum_ExternalAttribution_Source source' _
  , _attributionConfidence = attributionConfidence'
  , _comment = comment'
  , _originId = originId'
  , _coordinates = coordinates'
  , _copyright = copyright'
  , _licenseName = licenseName'
  , _licenseText = licenseText'
  , _preselected = preselected'
  }) 
 = if (and [ source == source'
           , coordinates == coordinates'
           , (cleanupLicense licenseName) == (cleanupLicense licenseName')])
   then Just (left{ _attributionConfidence = (attributionConfidence `min` attributionConfidence')
                  , _copyright = mergifyCopyright copyright copyright'
                  , _licenseName = cleanupLicense licenseName
                  , _licenseText = case licenseText of
                    Just lt -> Just lt
                    _       -> licenseText'
                  , _preselected = preselected || preselected'
                  })
   else Nothing

clusterifyEAMap :: Map.Map UUID Opossum_ExternalAttribution -> [(UUID, Opossum_ExternalAttribution, [UUID])]
clusterifyEAMap = let
        clusterifyEAMap' :: [(UUID, Opossum_ExternalAttribution)]
                         -> [(UUID, Opossum_ExternalAttribution, [UUID])]
                         -> [(UUID, Opossum_ExternalAttribution, [UUID])]
        clusterifyEAMap' [] out               = out
        clusterifyEAMap' (i:ins) out = let
            clusterifyEAMap'' :: (UUID, Opossum_ExternalAttribution)
                             -> [(UUID, Opossum_ExternalAttribution, [UUID])]
                             -> [(UUID, Opossum_ExternalAttribution, [UUID])]
            clusterifyEAMap'' (uuid, ea) []                              = [(uuid, ea, [])]
            clusterifyEAMap'' (uuid, ea) (ins@(in'@(uuid', ea', uuids):ins')) 
                | uuid == uuid' = ins
                | otherwise     = case mergifyEA ea ea' of
                    Just mergedEA -> (uuid', mergedEA, uuid : uuids):ins'
                    Nothing -> in' : (clusterifyEAMap'' (uuid, ea) ins')
            in clusterifyEAMap' ins (clusterifyEAMap'' i out)
      
    in (`clusterifyEAMap'` []) . Map.assocs

clusterifyOpossum :: Opossum -> Opossum
clusterifyOpossum (opossum@Opossum { _externalAttributions = eas , _resourcesToAttributions = rtas }) = let
    clusters = clusterifyEAMap eas
    newEas = (Map.fromList . map (\(uuid, ea, _) -> (uuid, ea))) clusters
    lookupMap = ( Map.fromList . concatMap (\(uuid, _, uuids) -> map (\uuid' -> (uuid', uuid)) uuids)) clusters
    newRtas = (Map.map (\uuids -> List.nub (map (\uuid -> Map.findWithDefault uuid uuid lookupMap) uuids))) rtas
  in opossum { _externalAttributions = newEas
         , _resourcesToAttributions = newRtas
         }

unDot :: Opossum -> Opossum
unDot (opossum@Opossum { _resources = rs , _resourcesToAttributions = rtas }) = let
  undotResources (rs@Opossum_Resources { _dirs = dirs }) = let
    contentOfDot = Map.findWithDefault mempty "." dirs
    dirsWithoutDot = Map.delete "." dirs
    in rs {_dirs = dirsWithoutDot} <> contentOfDot
  undotRTAS = Map.mapKeys (\k -> case List.stripPrefix "/." k of
    Nothing -> k
    Just k' -> k')
  in opossum{ _resources = undotResources rs , _resourcesToAttributions = undotRTAS rtas }

dropDir :: FilePath -> Opossum -> Opossum
dropDir directoryName (opossum@Opossum{ _resources = rs, _resourcesToAttributions = rtas}) = let
  filterResources (rs@Opossum_Resources { _dirs = dirs }) = let
    filteredDirs = Map.filterWithKey (\key -> const (not (directoryName == key))) dirs
    filteredAndCleanedDirs = Map.map filterResources filteredDirs
    in rs{ _dirs = filteredAndCleanedDirs }
  filterRTAS = Map.filterWithKey (\key -> const (not (directoryName `List.isSubsequenceOf` key))) 
  in opossum{ _resources = filterResources rs, _resourcesToAttributions = filterRTAS rtas}

parseOpossum :: FP.FilePath -> IO Opossum
parseOpossum fp = do
  hPutStrLn IO.stderr ("parse: " ++ fp)
  bs <- (if (FP.takeExtension fp) == ".gz"
         then GZip.decompress
         else id) <$> B.readFile fp
  case A.eitherDecode' bs of
    Right opossum -> return opossum
    Left err  -> do
        fail err

computeMergedOpossum :: [FilePath] -> IO B.ByteString
computeMergedOpossum inputPaths = do
    opossums <- mapM parseOpossum inputPaths
    let finalOpossum = clusterifyOpossum $ mconcat (map unDot opossums)
    return (A.encodePretty finalOpossum)
