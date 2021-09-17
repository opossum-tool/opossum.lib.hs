-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Opossum.OpossumScanossUtils
  ( parseScanossToOpossum
  ) where

import           Opossum.Opossum
import           Opossum.OpossumUtils
import           PURL.PURL

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import           Data.List                      ( intercalate )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Distribution.Parsec           as SPDX
import qualified Distribution.SPDX             as SPDX
import           SPDX.Document.Common           ( parseLicenses
                                                , renderSpdxLicense
                                                )
import qualified System.FilePath               as FP
import           System.IO                      ( Handle
                                                , hClose
                                                , hPutStrLn
                                                , stdout
                                                )
import qualified System.IO                     as IO
import           System.Random                  ( randomIO )

import           Debug.Trace                    ( trace )

{-
        {
          "vendor": "Ezekiel",
          "component": "rollup-plugin-uglify-es",
          "version": "0.0.1",
          "source": "component_declared"
        },
-}
data ScanossFindingDependency = ScanossFindingDependency
  {
          _ScanossFindingDependency_vendor :: String
         , _ScanossFindingDependency_component :: String
         , _ScanossFindingDependency_version :: String
         , _ScanossFindingDependency_source :: String

  }

{-
{
  "./yarn.lock": [
    {
      "id": "snippet",
      "status": "pending",
      "lines": "13277-13789,12930-13064,12556-12851,12257-12371,11605-11973",
      "oss_lines": "15730-16242,15002-15136,14490-14785,14292-14406,13579-13947",
      "matched": "10%",
      "purl": [
        "pkg:github/leclercb/taskunifier-app"
      ],
      "vendor": "leclercb",
      "component": "taskunifier-app",
      "version": "1.2.9",
      "latest": "1.2.9",
      "url": "https://github.com/leclercb/taskunifier-app",
      "release_date": "2021-07-22",
      "file": "yarn.lock",
      "url_hash": "aee3ccf9020c05567ace82879a2ab64d",
      "file_hash": "afaf765786c8dbdd11acd7b01fb25ee3",
      "file_url": "https://osskb.org/api/file_contents/afaf765786c8dbdd11acd7b01fb25ee3",
      "dependencies": [
        {
          "vendor": "Ezekiel",
          "component": "rollup-plugin-uglify-es",
          "version": "0.0.1",
          "source": "component_declared"
        },
        {
          "vendor": "Fractal",
          "component": "gulp-util",
          "version": "^3.0.8",
          "source": "component_declared"
        },
      ],
      "licenses": [
        {
          "name": "AGPL-3.0",
          "source": "component_declared"
        }
      ],
      "copyrights": [
        {
          "name": "Copyright .. 2012...2016 Kir Belevich",
          "source": "license_file"
        },
        {
          "name": "Copyright 2012-2018 (c) Mihai Bazon <mihai.bazon@gmail.com>",
          "source": "license_file"
        },
        {
          "name": "copyright owner or entity authorized by",
          "source": "license_file"
        }
      ],
      "vulnerabilities": [],
      "quality": [],
      "cryptography": [],
      "server": {
        "hostname": "p10",
        "version": "4.2.9",
        "flags": "0",
        "elapsed": "1.110124s"
      }
    }
  ],
  "./scripts/fossid-to-opossum-converter/process_larger_fossid_json.py": [
    {
      "id": "none",
      "server": {
        "hostname": "p8",
        "version": "4.2.9",
        "flags": "0",
        "elapsed": "0.280415s"
      }
    }
  ],
  ...
-}
data ScanossFinding = ScanossFinding
  { _ScanossFinding_id :: String
  , _ScanossFinding_matched :: String
  , _ScanossFinding_purl :: [PURL]
  , _ScanossFinding_vendor :: Maybe String
  , _ScanossFinding_component :: Maybe String
  , _ScanossFinding_version :: Maybe String
  , _ScanossFinding_latest :: Maybe String
  , _ScanossFinding_url :: Maybe String
  -- , _ScanossFinding_release_date": "2021-07-22",
  -- , _ScanossFinding_file": "yarn.lock",
  -- , _ScanossFinding_url_hash": "aee3ccf9020c05567ace82879a2ab64d",
  -- , _ScanossFinding_file_hash": "afaf765786c8dbdd11acd7b01fb25ee3",
  , _ScanossFinding_file_url :: Maybe String
  , _ScanossFinding_dependencies :: [ScanossFindingDependency]
  , _ScanossFinding_licenses :: [LicenseExpression]
  , _ScanossFinding_copyrights :: [String]
  -- , _ScanossFinding_vulnerabilities": [],
  -- , _ScanossFinding_quality": [],
  -- , _ScanossFinding_cryptography": [],
  }
















instance A.FromJSON ScanossPackage where
  parseJSON = A.withObject "ScanossPackage" $ \v -> do
    purl <-
      v
      A..:? "purl"
      >>=   (\case
              Just purl -> return $ parsePURL purl
              Nothing   -> return Nothing
            )
    dependencies <-
      (     v
      A..:? "dependencies"
      >>=   (\case
              Just dependencies -> mapM A.parseJSON dependencies
              Nothing           -> return []
            )
      ) :: A.Parser [ScanossPackage]
    license <-
      v
      A..:? "license_expressions"
      >>=   (\case
              Just lics -> return $ parseLicenses lics
              Nothing   -> return Nothing
            )
    copyright <- v A..:? "copyright"
    return $ ScanossPackage purl license copyright dependencies
data ScanossFileEntry = ScanossFileEntry
  { _scfe_file       :: FilePath
  , _scfe_is_file    :: Bool
  , _scfe_license    :: Maybe SPDX.LicenseExpression
  , _scfe_copyrights :: [String]
  , _scfe_packages   :: [ScanossPackage]
  }
  deriving (Eq, Show)
instance A.FromJSON ScanossFileEntry where
  parseJSON = A.withObject "ScanossFileEntry" $ \v -> do
    path    <- v A..: "path"
    is_file <-
      (\case
        ("file" :: String) -> True
        _                  -> False
      )
      <$>  v
      A..: "type"
    -- sha1 <- v `getHash` "sha1"
    -- md5 <- v `getHash` "md5"
    -- sha256 <- v `getHash` "sha256"
    -- sha512 <- v `getHash` "sha512"
    -- let idFromHashes = mconcat $ sha1 ++ md5 ++ sha256 ++ sha512
    license <-
      v
      A..:? "license_expressions"
      >>=   (\case
              Just lics -> return $ parseLicenses lics
              Nothing   -> return Nothing
            )
    copyrights <- do
      listOfCopyrightObjects <-
        (v A..:? "copyrights" :: A.Parser (Maybe A.Array))
      case listOfCopyrightObjects of
        Just cos ->
          let getValueFromCopyrightObject = A.withObject "CopyrightsEntry"
                $ \v' -> v' A..: "value" :: A.Parser String
          in  mapM getValueFromCopyrightObject (V.toList cos)
        Nothing -> return []
    copyrights <- return []
    packages   <- v A..: "packages"
    return (ScanossFileEntry path is_file license copyrights packages)

data ScanossFile = ScanossFile
  { _scf_files :: [ScanossFileEntry]
  }
  deriving (Eq, Show)
instance A.FromJSON ScanossFile where
  parseJSON = A.withObject "ScanossFile" $ \v -> do
    ScanossFile <$> v A..: "files"


opossumFromScanossPackage :: ScanossPackage -> IO Opossum
opossumFromScanossPackage (ScanossPackage { _scp_purl = purl, _scp_licenses = licenses, _scp_copyright = copyright, _scp_dependencies = dependencies })
  = let
      typeFromPurl = case purl of
        Just (PURL { _PURL_type = t }) -> "generic" `fromMaybe` (fmap show t)
        _                              -> "generic"
      pathFromPurl = typeFromPurl FP.</> case purl of
        Just (PURL { _PURL_namespace = ns, _PURL_name = n, _PURL_version = v })
          -> foldl1 (FP.</>)
            $  (maybeToList ns)
            ++ [(intercalate "@" $ [n] ++ (maybeToList v))]
        _ -> "UNKNOWN"
      coordinatesFromPurl = case purl of
        Just (PURL { _PURL_namespace = ns, _PURL_name = n, _PURL_version = v })
          -> Opossum_Coordinates ((Just . T.pack) typeFromPurl)
                                 (fmap T.pack ns)
                                 ((Just . T.pack) n)
                                 (fmap T.pack v)
        _ -> Opossum_Coordinates Nothing Nothing Nothing Nothing
    in
      do
        uuid <- randomIO
        let source    = Opossum_ExternalAttribution_Source "Scanoss-Package" 50
        let resources = fpToResources True pathFromPurl
        let ea = Opossum_ExternalAttribution
              source
              50
              Nothing
              Nothing
              coordinatesFromPurl
              (fmap T.pack copyright)
              (fmap (T.pack . renderSpdxLicense) licenses)
              Nothing
              False

        let
          o = mempty
            { _resources               = resources
            , _externalAttributions    = (Map.singleton uuid ea)
            , _resourcesToAttributions =
              (Map.singleton ("/" FP.</> pathFromPurl) [uuid])
            , _attributionBreakpoints  = Set.singleton
                                           ("/" ++ typeFromPurl ++ "/")
            }
        os <- mapM opossumFromScanossPackage dependencies
        return $ mconcat (o : (map (unshiftPathToOpossum pathFromPurl) os))

scancodeFileEntryToOpossum :: ScanossFileEntry -> IO Opossum
scancodeFileEntryToOpossum (ScanossFileEntry { _scfe_file = path, _scfe_is_file = is_file, _scfe_license = licenses, _scfe_copyrights = copyrights, _scfe_packages = packages })
  = let
      filesWithChildren =
        if is_file then Set.singleton ("/" FP.</> path ++ "/") else mempty
      opossumFromLicenseAndCopyright = do
        uuid <- randomIO
        let resources = fpToResources True path
        if null copyrights && licenses == Nothing
          then return $ mempty { _resources         = resources
                               , _filesWithChildren = filesWithChildren
                               }
          else do
            let source = Opossum_ExternalAttribution_Source "Scanoss" 50
            let ea = Opossum_ExternalAttribution
                  source
                  50
                  Nothing
                  Nothing
                  (Opossum_Coordinates Nothing Nothing Nothing Nothing)
                  ((Just . T.pack . unlines) copyrights)
                  (fmap (T.pack . renderSpdxLicense) licenses)
                  Nothing
                  False
            return $ mempty
              { _resources               = resources
              , _externalAttributions    = (Map.singleton uuid ea)
              , _resourcesToAttributions = (Map.singleton ("/" FP.</> path)
                                                          [uuid]
                                           )
              , _filesWithChildren       = filesWithChildren
              }
    in
      do
        o  <- opossumFromLicenseAndCopyright
        os <- mapM opossumFromScanossPackage packages
        return $ mconcat (o : (map (unshiftPathToOpossum path) os))

parseScanossBS :: B.ByteString -> IO Opossum
parseScanossBS bs = case (A.eitherDecode bs :: Either String (Map.Map String [ScanossFinding])) of
  Right (ScanossFile scFiles) ->
    mconcat $ map scancodeFileEntryToOpossum scFiles
  Left err -> do
    putStrLn err
    undefined -- TODO

parseScanossToOpossum :: FilePath -> IO Opossum
parseScanossToOpossum inputPath = do
  hPutStrLn IO.stderr ("parse: " ++ inputPath)
  let baseOpossum = mempty
        { _metadata = (Just
                        (A.object
                          [ "projectId" A..= ("0" :: String)
                          , "projectTitle" A..= inputPath
                          , "fileCreationDate" A..= ("" :: String)
                          ]
                        )
                      )
        }
  opossum <- B.readFile inputPath >>= parseScanossBS
  return (normaliseOpossum (baseOpossum <> opossum))
