{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Opossum.OpossumSPDXUtils
  ( spdxToOpossum
  , parseSpdxToOpossum
  ) where

import Opossum.Opossum

import SPDX.Document

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
import qualified Data.Maybe as Maybe
import System.Random (randomIO)
import           System.FilePath
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.ByteString.Lazy.Char8 as C8

spdxToOpossum :: SPDXDocument -> IO Opossum
spdxToOpossum = let
    spdxFileToEA :: SPDXFile -> Opossum_ExternalAttribution
    spdxFileToEA (SPDXFile 
      { _SPDXFile_SPDXID = spdxid
      , _SPDXFile_raw = raw
      , _SPDXFile_fileName = filename
      , _SPDXFile_fileTypes = _
      , _SPDXFile_checksums = _
      , _SPDXFile_LicenseConcluded = license
      , _SPDXFile_licenseInfoInFiles = _
      , _SPDXFile_licenseInfoFromFiles = _
      , _SPDXFile_licenseComments = _
      , _SPDXFile_copyrightText = copyright
      , _SPDXFile_comment = _
      , _SPDXFile_noticeText = notice
      , _SPDXFile_fileContributors = _
      , _SPDXFile_attributionTexts = attribution
      , _SPDXFile_fileDependencies = dependencies
      , _SPDXFile_name = name
      }) = Opossum_ExternalAttribution
            { _source = Opossum_ExternalAttribution_Source "SPDXFile" 100
            , _attributionConfidence = 100
            , _comment = (Just . T.pack . C8.unpack . A.encodePretty) raw
            , _originId = Nothing
            , _coordinates = Opossum_Coordinates Nothing Nothing Nothing Nothing
            , _copyright = Just $ T.pack copyright 
            , _licenseName = Just $ T.pack $ show license -- TODO
            , _licenseText = Nothing -- TODO
            , _preselected = False
            }

    spdxPackageToEA :: SPDXPackage -> Opossum_ExternalAttribution
    spdxPackageToEA (SPDXPackage 
      { _SPDXPackage_SPDXID = spdxid
      , _SPDXPackage_raw = raw
      , _SPDXPackage_name = name
      , _SPDXPackage_versionInfo = version
      , _SPDXPackage_packageFileName = _
      , _SPDXPackage_supplier = _
      , _SPDXPackage_originator = _
      , _SPDXPackage_downloadLocation = _
      , _SPDXPackage_filesAnalyzed = _
      , _SPDXPackage_packageVerificationCode = _
      , _SPDXPackage_checksums = _
      , _SPDXPackage_homepage = _
      , _SPDXPackage_sourceInfo = _
      , _SPDXPackage_licenseConcluded = _
      , _SPDXPackage_licenseInfoFromFiles = _
      , _SPDXPackage_licenseDeclared = license
      , _SPDXPackage_licenseComments = _
      , _SPDXPackage_copyrightText = copyright
      , _SPDXPackage_summary = _
      , _SPDXPackage_description = _
      , _SPDXPackage_comment = _
      , _SPDXPackage_attributionTexts = _
      , _SPDXPackage_hasFiles = _
      }) = Opossum_ExternalAttribution
          { _source = Opossum_ExternalAttribution_Source "SPDXPackage" 100
          , _attributionConfidence = 100
          , _comment = (Just . T.pack . C8.unpack . A.encodePretty) raw
          , _originId = Nothing
          , _coordinates = Opossum_Coordinates Nothing Nothing ((Just . T.pack) name) (fmap T.pack version)
          , _copyright = case copyright of
            SPDXJust copyright' -> (Just . T.pack)  copyright'
            _                   -> Nothing
          , _licenseName = Just $ T.pack $ show license -- TODO
          , _licenseText = Nothing -- TODO
          , _preselected = True
          } 

    spdxFileOrPackageToEA :: Either SPDXFile SPDXPackage -> Opossum_ExternalAttribution
    spdxFileOrPackageToEA (Left f) = spdxFileToEA f
    spdxFileOrPackageToEA (Right p) = spdxPackageToEA p
    in \(spdx@SPDXDocument
      { _SPDX_SPDXID = spdxid
      , _SPDX_comment = _
      , _SPDX_creationInfo = _
      , _SPDX_name = name
      , _SPDX_documentDescribes = roots
      , _SPDX_files = files
      , _SPDX_packages = packages
      , _SPDX_relationships = relationships
      }) -> let
        opossumMetadata = mempty { _metadata = Just (A.object ["projectId" A..= spdxid, "projectTitle" A..= name]) }
        (graph, idsToIdxs, indxsToIds) = spdxDocumentToGraph spdx
        trees = map (\root -> let
          indx = Map.findWithDefault (undefined) root idsToIdxs
          in G.lbft indx graph :: [G.LPath SPDXRelationship])
          roots
        inits = let
          addS :: [G.LNode SPDXRelationship] -> Maybe ([G.LNode SPDXRelationship],  Either SPDXFile SPDXPackage)
          addS [] = Nothing
          addS path = let
              (node,_) = last path
            in case G.lab graph node of 
              Just s -> Just (path, s)
              _ -> Nothing
          in concatMap (Maybe.catMaybes . map addS . List.nub . concatMap List.inits . map (reverse . G.unLPath)) trees
        initToOpossum :: ([G.LNode SPDXRelationship],  Either SPDXFile SPDXPackage) -> IO Opossum
        initToOpossum (p, s) = let
          ea = spdxFileOrPackageToEA s
          rs = ('/':) . joinPath $ map ((\k -> case G.lab graph k of
            Just (Left (SPDXFile {_SPDXFile_SPDXID = spdxid, _SPDXFile_name = name})) -> case name of
              Just name'  -> name'
              _ -> spdxid
            Just (Right (SPDXPackage {_SPDXPackage_SPDXID = spdxid, _SPDXPackage_name = name})) -> case name of
              "" -> spdxid
              _  -> name
            Nothing -> "??") . fst) p
          rsFull = case s of 
            Left (SPDXFile{_SPDXFile_fileName = fn}) -> rs </> fn
            _      -> rs ++ "/"
          in do
            uuid <- randomIO
            return $ mempty
              { _resources = fpToResources (case s of 
                Left _ -> True 
                _      -> False) rsFull
              , _externalAttributions = Map.singleton uuid ea
              , _resourcesToAttributions = Map.singleton rsFull [uuid]
              }
      in do
        pieces <- mapM initToOpossum inits
        return $ mconcat (opossumMetadata : pieces)

parseSpdxToOpossum :: FilePath -> IO B.ByteString
parseSpdxToOpossum inputPath = do
  spdx <- parseSPDXDocument inputPath
  opossum <- spdxToOpossum spdx
  return (A.encodePretty opossum)