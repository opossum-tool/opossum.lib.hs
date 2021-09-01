{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDX.SPDX
  ( module X
  , SPDXDocument (..)
  , parseSPDXDocument, parseSPDXDocumentBS
  , spdxDocumentToGraph
  ) where

import YACP.Core
import YACP.ParserHelper

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as UG

import YACP.SPDX.Common as X
import YACP.SPDX.DocumentCreationInformation as X
import YACP.SPDX.PackageInformation as X
import YACP.SPDX.FileInformation as X
import YACP.SPDX.SnippetInformation as X
import YACP.SPDX.OtherLicensingInformationDetected as X
import YACP.SPDX.Annotations as X
import YACP.SPDX.RelationshipsbetweenSPDXElements as X

data SPDXDocument
  = SPDXDocument
  { _SPDX_SPDXID :: SPDXID
  , _SPDX_comment :: Maybe String
  , _SPDX_spdxVersion :: String
  , _SPDX_creationInfo :: SPDXCreationInfo
  , _SPDX_name :: String
  , _SPDX_dataLicense :: String
  , _SPDX_documentDescribes :: [SPDXID]
  -- , _SPDX_describesPackages :: [SPDXID]

  , _SPDX_files :: [SPDXFile]
  , _SPDX_packages :: [SPDXPackage]
  , _SPDX_relationships :: [SPDXRelationship]
  } deriving (Eq, Show)
instance A.FromJSON SPDXDocument where
  parseJSON = A.withObject "SPDXDocument" $ \v ->
    SPDXDocument
    <$> v A..: "SPDXID"
    <*> v A..:? "comment"
    <*> v A..: "spdxVersion"
    -- <*> v A..: "externalDocumentRefs"
    <*> v A..: "creationInfo"
    <*> v A..: "name"
    <*> v A..: "dataLicense"
    <*> v A..: "documentDescribes" -- or "describesPackages"

    <*> fmap ([] `Maybe.fromMaybe`) (v A..:? "files")
    <*> fmap ([] `Maybe.fromMaybe`) (v A..: "packages")
    -- <*> v A..: "hasExtractedLicensingInfos"
    -- <*> v A..: "snippets"
    <*> fmap ([] `Maybe.fromMaybe`) (v A..:? "relationships")
    -- <*> v A..: "revieweds"
    -- <*> v A..: "annotations"

parseSPDXDocument :: FilePath -> IO SPDXDocument
parseSPDXDocument p = do
  bs <- B.readFile p
  case parseSPDXDocumentBS bs of 
    Left err -> fail err
    Right spdx -> return spdx
parseSPDXDocumentBS :: B.ByteString -> Either String SPDXDocument
parseSPDXDocumentBS bs = case A.eitherDecode' bs of
  Right spdx -> Right spdx
  Left err   -> case Y.decodeEither' (B.toStrict bs) of
    Right spdx -> Right spdx
    Left err'  -> Left (show [err, show err'])

spdxDocumentToGraph :: SPDXDocument 
                    -> (UG.Gr (Either SPDXFile SPDXPackage) SPDXRelationship
                       , Map.Map String Int
                       , Map.Map Int String)
spdxDocumentToGraph = let
    spdxFileToNode :: Map.Map String Int -> SPDXFile -> G.LNode (Either SPDXFile SPDXPackage)
    spdxFileToNode idsToIdxs (f@SPDXFile { _SPDXFile_SPDXID = spdxid}) =
      (Map.findWithDefault (-1) spdxid idsToIdxs, Left f) 

    spdxPackageToNode :: Map.Map String Int -> SPDXPackage -> G.LNode (Either SPDXFile SPDXPackage)
    spdxPackageToNode idsToIdxs (p@SPDXPackage { _SPDXPackage_SPDXID = spdxid }) =
       (Map.findWithDefault (-1) spdxid idsToIdxs, Right p) 
       
    spdxRelationToEdge :: Map.Map String Int -> SPDXRelationship -> G.LEdge SPDXRelationship
    spdxRelationToEdge idsToIdxs (r@SPDXRelationship
      { _SPDXRelationship_comment = _
      , _SPDXRelationship_relationshipType = _
      , _SPDXRelationship_relatedSpdxElement = source
      , _SPDXRelationship_spdxElementId = target
      }) = (Map.findWithDefault (-1) source idsToIdxs, Map.findWithDefault (-1) target idsToIdxs, r)
  in \(SPDXDocument
  { _SPDX_comment = _
  , _SPDX_creationInfo = _
  , _SPDX_name = name
  , _SPDX_documentDescribes = roots
  , _SPDX_files = files
  , _SPDX_packages = packages
  , _SPDX_relationships = relationships
  }) -> let
    idsToIdxs = Map.fromList (zip (List.nub $ roots ++ map _SPDXPackage_SPDXID packages ++ map _SPDXFile_SPDXID files) [1..])
    nodes = let
        nodesFromFiles = map (spdxFileToNode idsToIdxs) files
        nodesFromPackages = map (spdxPackageToNode idsToIdxs) packages
      in nodesFromFiles ++ nodesFromPackages
    nodesMap = Map.fromList nodes
    edges = map (spdxRelationToEdge idsToIdxs) relationships
  in (G.mkGraph nodes edges, idsToIdxs, (Map.fromList . map (\(k,v) -> (v,k)) . Map.toList) idsToIdxs)
