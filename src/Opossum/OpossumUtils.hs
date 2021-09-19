-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
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
  , normaliseOpossum
  , dropDir
  , parseOpossum
  , mergifyEA
  , opossumFromFileTree
  , unshiftPathToResources
  , unshiftPathToOpossum
  ) where

import           Opossum.Opossum

import qualified Codec.Compression.GZip        as GZip
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.UUID                      ( UUID )
import qualified Data.Vector                   as V
import           System.Directory               ( withCurrentDirectory )
import           System.Directory.Extra         ( listFilesRecursive )
import qualified System.FilePath               as FP
import           System.FilePath                ( (</>) )
import           System.IO                      ( Handle
                                                , hClose
                                                , hPutStrLn
                                                , stdout
                                                )
import qualified System.IO                     as IO
import           System.Random                  ( randomIO )
import           Text.Regex                     ( mkRegex
                                                , subRegex
                                                )

mergifyCopyright :: Maybe Text -> Maybe Text -> Maybe Text
mergifyCopyright left    Nothing = left
mergifyCopyright Nothing right   = right
mergifyCopyright (Just c) (Just c') =
  Just . T.unlines . List.nub $ (T.lines c) ++ (T.lines c')

cleanupLicense :: Maybe Text -> Maybe Text
cleanupLicense Nothing   = Nothing
cleanupLicense (Just "") = Nothing
cleanupLicense (Just t ) = case T.stripPrefix ", " t of
  Nothing -> Just t
  t'      -> t'

mergifyEA
  :: Opossum_ExternalAttribution
  -> Opossum_ExternalAttribution
  -> Maybe Opossum_ExternalAttribution
mergifyEA left@(Opossum_ExternalAttribution { _source = Opossum_ExternalAttribution_Source source _, _attributionConfidence = attributionConfidence, _comment = comment, _originId = originId, _coordinates = coordinates, _copyright = copyright, _licenseName = licenseName, _licenseText = licenseText, _flags = flags }) (right@Opossum_ExternalAttribution { _source = Opossum_ExternalAttribution_Source source' _, _attributionConfidence = attributionConfidence', _comment = comment', _originId = originId', _coordinates = coordinates', _copyright = copyright', _licenseName = licenseName', _licenseText = licenseText', _flags = flags' })
  = if left
       == right
       || (and
            [ source == source'
            , coordinatesAreNotNull coordinates
            , coordinates == coordinates'
            , (cleanupLicense licenseName) == (cleanupLicense licenseName')
            ]
          )
    then Just
      (left
        { _attributionConfidence = (     attributionConfidence
                                   `min` attributionConfidence'
                                   )
        , _copyright             = mergifyCopyright copyright copyright'
        , _licenseName           = cleanupLicense licenseName
        , _licenseText           = case licenseText of
                                     Just lt -> Just lt
                                     _       -> licenseText'
        , _flags                 = flags <> flags'
        }
      )
    else Nothing

clusterifyEAMap
  :: Map.Map UUID Opossum_ExternalAttribution
  -> [(UUID, Opossum_ExternalAttribution, [UUID])]
clusterifyEAMap =
  let clusterifyEAMap'
        :: [(UUID, Opossum_ExternalAttribution)]
        -> [(UUID, Opossum_ExternalAttribution, [UUID])]
        -> [(UUID, Opossum_ExternalAttribution, [UUID])]
      clusterifyEAMap' [] out = out
      clusterifyEAMap' (i : ins) out =
        let clusterifyEAMap''
              :: (UUID, Opossum_ExternalAttribution)
              -> [(UUID, Opossum_ExternalAttribution, [UUID])]
              -> [(UUID, Opossum_ExternalAttribution, [UUID])]
            clusterifyEAMap'' (uuid, ea) [] = [(uuid, ea, [])]
            clusterifyEAMap'' (uuid, ea) (ins@(in'@(uuid', ea', uuids) : ins'))
              | uuid == uuid' = ins
              | otherwise = case mergifyEA ea ea' of
                Just mergedEA -> (uuid', mergedEA, uuid : uuids) : ins'
                Nothing       -> in' : (clusterifyEAMap'' (uuid, ea) ins')
        in  clusterifyEAMap' ins (clusterifyEAMap'' i out)
  in  (`clusterifyEAMap'` []) . Map.assocs

clusterifyOpossum :: Opossum -> Opossum
clusterifyOpossum (opossum@Opossum { _externalAttributions = eas, _resourcesToAttributions = rtas })
  = let
      clusters = clusterifyEAMap eas
      newEas   = (Map.fromList . map (\(uuid, ea, _) -> (uuid, ea))) clusters
      lookupMap =
        (Map.fromList . concatMap
            (\(uuid, _, uuids) -> map (\uuid' -> (uuid', uuid)) uuids)
          )
          clusters
      newRtas =
        (Map.map
            (\uuids -> List.nub
              (map (\uuid -> Map.findWithDefault uuid uuid lookupMap) uuids)
            )
          )
          rtas
    in
      opossum { _externalAttributions    = newEas
              , _resourcesToAttributions = newRtas
              }

unDot :: Opossum -> Opossum
unDot (opossum@Opossum { _resources = rs, _resourcesToAttributions = rtas }) =
  let
    undotResources :: Opossum_Resources -> Opossum_Resources
    undotResources (rs@Opossum_Resources { _dirs = dirs }) =
      let contentOfDot   = Map.findWithDefault mempty "." dirs
          dirsWithoutDot = Map.delete "." dirs
          recurse :: Opossum_Resources -> Opossum_Resources
          recurse (rs@Opossum_Resources { _dirs = dirs }) =
            rs { _dirs = Map.map undotResources dirs }
      in  recurse $ rs { _dirs = dirsWithoutDot } <> contentOfDot
    undotRTAS = Map.mapKeys
      (\path -> FP.normalise (subRegex (mkRegex "/(\\.?/)+") path "/"))
  in
    opossum { _resources               = undotResources rs
            , _resourcesToAttributions = undotRTAS rtas
            }

normaliseOpossum :: Opossum -> Opossum
normaliseOpossum opossum = case unDot opossum of
  opossum'@Opossum { _resources = rs, _resourcesToAttributions = rtas, _attributionBreakpoints = abs, _filesWithChildren = fwcs }
    -> let normaliseId :: FilePath -> FilePath
           normaliseId fp = FP.normalise
             $ if isPathAFileInResources fp rs then fp else fp ++ "/"
       in  opossum'
             { _resourcesToAttributions = Map.mapKeysWith (++) normaliseId rtas
             , _attributionBreakpoints  = Set.map normaliseId abs
             , _filesWithChildren       = Set.map normaliseId fwcs
             }

dropDir :: FilePath -> Opossum -> Opossum
dropDir directoryName (opossum@Opossum { _resources = rs, _resourcesToAttributions = rtas })
  = let filterResources (rs@Opossum_Resources { _dirs = dirs }) =
          let filteredDirs = Map.filterWithKey
                (\key -> const (not (directoryName == key)))
                dirs
              filteredAndCleanedDirs = Map.map filterResources filteredDirs
          in  rs { _dirs = filteredAndCleanedDirs }
        filterRTAS = Map.filterWithKey
          (\key -> const (not (directoryName `List.isSubsequenceOf` key)))
    in  opossum { _resources               = filterResources rs
                , _resourcesToAttributions = filterRTAS rtas
                }

unshiftPathToResources :: FilePath -> Opossum_Resources -> Opossum_Resources
unshiftPathToResources prefix resources =
  let unshiftPathToResources'
        :: [FilePath] -> Opossum_Resources -> Opossum_Resources
      unshiftPathToResources' []       = id
      unshiftPathToResources' (p : ps) = \rs -> Opossum_Resources
        (Map.singleton p (unshiftPathToResources' ps rs))
        Set.empty
  in  ( (`unshiftPathToResources'` resources)
      . (map FP.dropTrailingPathSeparator)
      . FP.splitPath
      )
        prefix

unshiftPathToOpossum :: FilePath -> Opossum -> Opossum
unshiftPathToOpossum prefix (opossum@Opossum { _resources = rs, _resourcesToAttributions = rtas, _attributionBreakpoints = abs, _filesWithChildren = fwcs, _baseUrlsForSources = bufss })
  = let rsWithPrefix   = unshiftPathToResources prefix rs
        unshiftToID    = FP.normalise . (("/" </> prefix ++ "/") ++)
    in  unDot $ opossum { _resources               = rsWithPrefix
                        , _resourcesToAttributions = Map.mapKeys unshiftToID rtas
                        , _attributionBreakpoints  = Set.map unshiftToID abs
                        , _filesWithChildren       = Set.map unshiftToID fwcs
                        , _baseUrlsForSources      = Map.mapKeys unshiftToID bufss
                        }

parseOpossum :: FP.FilePath -> IO Opossum
parseOpossum fp = do
  hPutStrLn IO.stderr ("parse: " ++ fp)
  bs <- (if (FP.takeExtension fp) == ".gz" then GZip.decompress else id)
    <$> B.readFile fp
  case A.eitherDecode' bs of
    Right opossum -> return opossum
    Left  err     -> do
      fail err

opossumFromFileTree :: FilePath -> IO Opossum
opossumFromFileTree dir = withCurrentDirectory dir $ do
  resources <- fpsToResources <$> listFilesRecursive "."
  (return . unDot) $ mempty { _resources = resources }
