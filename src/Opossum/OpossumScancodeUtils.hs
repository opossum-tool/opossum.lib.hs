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
module Opossum.OpossumScancodeUtils
  ( parseScancodeToOpossum
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
      "path": "Cargo.lock",
      "type": "file",
      "name": "Cargo.lock",
      "base_name": "Cargo",
      "extension": ".lock",
      "size": 35114,
      "date": "2021-01-18",
      "sha1": "c3f1ca217637c6edec185df11cfdddbba1d3cac3",
      "md5": "c78cc22b4e1d5e6dade34aeb592c73f9",
      "sha256": "c2a6153229d3aad680248b0cbedf9cbc3cbfff5586be360e5f7efbb8dceca8cc",
      "mime_type": "text/plain",
      "file_type": "ASCII text",
      "programming_language": null,
      "is_binary": false,
      "is_text": true,
      "is_archive": false,
      "is_media": false,
      "is_source": false,
      "is_script": false,
      "licenses": [],
      "license_expressions": [],
      "percentage_of_license_text": 0,
      "copyrights": [],
      "holders": [],
      "authors": [],
      "packages": [
        {
          "type": "cargo",
          "namespace": null,
          "name": null,
          "version": null,
          "qualifiers": {},
          "subpath": null,
          "primary_language": "Rust",
          "description": null,
          "release_date": null,
          "parties": [],
          "keywords": [],
          "homepage_url": null,
          "download_url": null,
          "size": null,
          "sha1": null,
          "md5": null,
          "sha256": null,
          "sha512": null,
          "bug_tracking_url": null,
          "code_view_url": null,
          "vcs_url": null,
          "copyright": null,
          "license_expression": null,
          "declared_license": null,
          "notice_text": null,
          "root_path": null,
          "dependencies": [
            {
              "purl": "pkg:crates/adler@0.2.3",
              "requirement": "0.2.3",
              "scope": "dependency",
              "is_runtime": true,
              "is_optional": false,
              "is_resolved": true
            },
            {
              "purl": "pkg:crates/aho-corasick@0.7.15",
              "requirement": "0.7.15",
              "scope": "dependency",
              "is_runtime": true,
              "is_optional": false,
              "is_resolved": true
            },
...
          ],
          "contains_source_code": null,
          "source_packages": [],
          "purl": null,
          "repository_homepage_url": null,
          "repository_download_url": null,
          "api_data_url": null
        }
-}
data ScancodePackage = ScancodePackage
  { _scp_purl         :: Maybe PURL
  , _scp_licenses     :: Maybe SPDX.LicenseExpression
  , _scp_copyright    :: Maybe String
  , _scp_dependencies :: [ScancodePackage]
  }
  deriving (Eq, Show)
instance A.FromJSON ScancodePackage where
  parseJSON = A.withObject "ScancodePackage" $ \v -> do
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
      ) :: A.Parser [ScancodePackage]
    license <-
      v
      A..:? "license_expressions"
      >>=   (\case
              Just lics -> return $ parseLicenses lics
              Nothing   -> return Nothing
            )
    copyright <- v A..:? "copyright"
    return $ ScancodePackage purl license copyright dependencies
data ScancodeFileEntry = ScancodeFileEntry
  { _scfe_file       :: FilePath
  , _scfe_is_file    :: Bool
  , _scfe_license    :: Maybe SPDX.LicenseExpression
  , _scfe_copyrights :: [String]
  , _scfe_packages   :: [ScancodePackage]
  }
  deriving (Eq, Show)
instance A.FromJSON ScancodeFileEntry where
  parseJSON = A.withObject "ScancodeFileEntry" $ \v -> do
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
    return (ScancodeFileEntry path is_file license copyrights packages)

data ScancodeFile = ScancodeFile
  { _scf_files :: [ScancodeFileEntry]
  }
  deriving (Eq, Show)
instance A.FromJSON ScancodeFile where
  parseJSON = A.withObject "ScancodeFile" $ \v -> do
    ScancodeFile <$> v A..: "files"


opossumFromScancodePackage :: ScancodePackage -> IO Opossum
opossumFromScancodePackage (ScancodePackage { _scp_purl = purl, _scp_licenses = licenses, _scp_copyright = copyright, _scp_dependencies = dependencies })
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
        let source    = Opossum_ExternalAttribution_Source "Scancode-Package" 50
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
        os <- mapM opossumFromScancodePackage dependencies
        return $ mconcat (o : (map (unshiftPathToOpossum pathFromPurl) os))

scancodeFileEntryToOpossum :: ScancodeFileEntry -> IO Opossum
scancodeFileEntryToOpossum (ScancodeFileEntry { _scfe_file = path, _scfe_is_file = is_file, _scfe_license = licenses, _scfe_copyrights = copyrights, _scfe_packages = packages })
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
            let source = Opossum_ExternalAttribution_Source "Scancode" 50
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
        os <- mapM opossumFromScancodePackage packages
        return $ mconcat (o : (map (unshiftPathToOpossum path) os))

parseScancodeBS :: B.ByteString -> IO Opossum
parseScancodeBS bs = case (A.eitherDecode bs :: Either String ScancodeFile) of
  Right (ScancodeFile scFiles) ->
    mconcat $ map scancodeFileEntryToOpossum scFiles
  Left err -> do
    putStrLn err
    undefined -- TODO

parseScancodeToOpossum :: FilePath -> IO Opossum
parseScancodeToOpossum inputPath = do
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
  opossum <- B.readFile inputPath >>= parseScancodeBS
  return (normaliseOpossum (baseOpossum <> opossum))
