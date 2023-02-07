-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Exception                   (evaluate)
import qualified Data.Aeson                          as A
import qualified Data.Aeson.Encode.Pretty            as A
import qualified Data.ByteString.Char8               as C
import qualified Data.ByteString.Lazy                as B
import           Data.Either
import           Data.FileEmbed                      (embedFile)
import qualified System.FilePath                     as FP
import qualified Data.List                           as List
import qualified Data.Map                            as Map
import           Data.Maybe                          (fromJust)
import qualified Data.Set                            as Set
import           Data.UUID
import qualified Data.Vector                         as V
import qualified Data.Yaml                           as Y
import           System.Exit
import           System.FilePath                     ((</>))
import           Test.Hspec
import           Test.QuickCheck

import           SPDX.Document

import           Opossum.Opossum
import           Opossum.OpossumDependencyCheckUtils
import           Opossum.OpossumSPDXUtils
import           Opossum.OpossumScancodeUtils
import           Opossum.OpossumScanossUtils
import           Opossum.OpossumUtils

opossumFileBS :: B.ByteString
opossumFileBS =
  B.fromStrict $(embedFile "test/data/zephyr-bundle.json.opossum.json")

dotOpossumFilePath :: FP.FilePath
dotOpossumFilePath = "test/data/synthetic_minimal_zipped_example.opossum"

spdxYamlFileBS :: B.ByteString
spdxYamlFileBS = B.fromStrict $(embedFile "test/data/document.spdx.yml")

spdxJsonFileBS :: B.ByteString
spdxJsonFileBS =
  B.fromStrict $(embedFile "test/data/SPDXJSONExample-v2.2.spdx.json")

spdxTernJsonFileBS :: B.ByteString
spdxTernJsonFileBS =
  B.fromStrict $(embedFile "test/data/httpd_2.4.51.tern.fixed.spdx.json")

scancodeJsonBS :: B.ByteString
scancodeJsonBS = B.fromStrict $(embedFile "test/data/tools-java.scancode.json")

scanpipeJsonBS :: B.ByteString
scanpipeJsonBS =
  B.fromStrict
    $(embedFile "test/data/docker __tomcat 10.0.6-jdk11-openjdk-buster v3.json")

dependencyCheckJsonBS :: B.ByteString
dependencyCheckJsonBS =
  B.fromStrict $(embedFile "test/data/dependency-check-report.json")

scanossJsonBS :: B.ByteString
scanossJsonBS = B.fromStrict $(embedFile "test/data/zlib_sca.json")

opossumSpec = do
  describe "Utils" $ do
    it "test uuidFromString" $ do
      uuidFromString "bla" `shouldBe` uuidFromString "bla"
      uuidFromString "bla" `shouldNotBe` uuidFromString "blubb"
  describe "Opossum Model" $
    let exmpResourses =
          [ "root" </> "subfolder" </> "file1"
          , "root" </> "subfolder" </> "file2"
          , "root" </> "file3"
          ]
        parsedResources =
          Resources
            (Map.singleton
               "root"
               (Resources
                  (Map.singleton
                     "subfolder"
                     (Resources Map.empty (Set.fromList ["file1", "file2"])))
                  (Set.singleton "file3")))
            Set.empty
        serializedResources =
          B.concat
            [ "{"
            , "\"root\":{"
            , "\"file3\":1,"
            , "\"subfolder\":{"
            , "\"file1\":1,"
            , "\"file2\":1"
            , "}"
            , "}"
            , "}"
            ]
        otherResources =
          Resources
            (Map.singleton
               "root"
               (Resources
                  (Map.singleton
                     "other"
                     (Resources Map.empty (Set.singleton "file4")))
                  Set.empty))
            (Set.singleton "file5")
        allResourcesSerialized =
          "{\"file5\":1,\"root\":{\"file3\":1,\"other\":{\"file4\":1},\"subfolder\":{\"file1\":1,\"file2\":1}}}"
     in do it "testFolderAndFileMerging" $ do
             (fpToResources True "path/to/file") <>
               (fpToResources False "path/other/dir") `shouldBe`
               Resources
                 (Map.singleton
                    "path"
                    (Resources
                       (Map.fromList
                          [ ("to", Resources Map.empty (Set.singleton "file"))
                          , ( "other"
                            , Resources (Map.singleton "dir" mempty) Set.empty)
                          ])
                       Set.empty))
                 Set.empty
             countFiles
               ((fpToResources True "path/to/file") <>
                (fpToResources False "path/other/dir")) `shouldBe`
               1
           it "test resourcesToPaths" $ do
             resourcesToPaths (Resources mempty mempty) `shouldBe`
               Set.fromList ["/"]
             resourcesToPaths (Resources mempty (Set.singleton "file.c")) `shouldBe`
               Set.fromList ["/", "file.c"]
             resourcesToPaths
               (Resources
                  (Map.singleton
                     "dir"
                     (Resources mempty (Set.singleton "something.h")))
                  (Set.singleton "file.c")) `shouldBe`
               Set.fromList ["/", "file.c", "dir/", "dir/something.h"]
           it "testFileListParsing" $ do
             fpsToResources exmpResourses `shouldBe` parsedResources
             (Set.fromList exmpResourses) `Set.isSubsetOf`
               (resourcesToPaths parsedResources) `shouldBe`
               True
           it "testFileListSerialization" $ do
             A.encode parsedResources `shouldBe` serializedResources
           it "testFileListDeSerialization" $ do
             A.decode serializedResources `shouldBe` (Just parsedResources)
           it "testMergeAndFileListSerialization" $ do
             A.encode (parsedResources <> otherResources) `shouldBe`
               allResourcesSerialized
  describe "Opossum Model deserialization" $
    let result = (A.eitherDecode opossumFileBS :: Either String Opossum)
        potentialError =
          case result of
            Right _  -> Nothing
            Left err -> Just err
        opossum =
          case result of
            Right opossum' -> opossum'
            Left _         -> undefined
     in do it "parsing of EA works, case 1" $ do
             let ea_str =
                   B.fromStrict $
                   C.unlines
                     [ "{"
                     , "    \"packageName\": \"adduser\","
                     , "    \"packageVersion\": \"3.118\","
                     , "    \"url\": \"https://github.com/some/repo\","
                     , "    \"licenseName\": \"MIT AND GPL-2.0-or-later\","
                     , "    \"copyright\": \"Some Copyright\","
                     , "    \"source\": {"
                     , "        \"name\": \"tern\","
                     , "        \"documentConfidence\": 100"
                     , "    }"
                     , "}"
                     ]
                 expected_source = ExternalAttribution_Source "tern" 100
                 expected_coordinates =
                   Coordinates
                     Nothing
                     Nothing
                     (Just "adduser")
                     (Just "3.118")
                     Nothing
                 expected_ea =
                   ExternalAttribution
                     expected_source
                     100
                     Nothing
                     Nothing
                     expected_coordinates
                     (Just "Some Copyright")
                     (Just "MIT AND GPL-2.0-or-later")
                     Nothing
                     (Just "https://github.com/some/repo")
                     Nothing
                     mempty
             ea <-
               case (A.eitherDecode ea_str :: Either String ExternalAttribution) of
                 Right ea' -> return ea'
                 Left err  -> fail err
             ea `shouldBe` expected_ea
           it "parsing of EA works, case 2" $ do
             let ea_str =
                   B.fromStrict $
                   C.unlines
                     [ "{"
                     , "    \"packageName\": \"adduser\","
                     , "    \"packageVersion\": \"3.118\","
                     , "    \"url\": \"\","
                     , "    \"licenseName\": \"\","
                     , "    \"copyright\": \"Some Copyright\","
                     , "    \"source\": {"
                     , "        \"name\": \"tern\","
                     , "        \"documentConfidence\": 100"
                     , "    },"
                     , "    \"preSelected\": true"
                     , "}"
                     ]
                 expected_source = ExternalAttribution_Source "tern" 100
                 expected_coordinates =
                   Coordinates
                     Nothing
                     Nothing
                     (Just "adduser")
                     (Just "3.118")
                     Nothing
                 expected_ea =
                   ExternalAttribution
                     expected_source
                     100
                     Nothing
                     Nothing
                     expected_coordinates
                     (Just "Some Copyright")
                     Nothing
                     Nothing
                     Nothing
                     Nothing
                     justPreselectedFlags
             ea <-
               case (A.eitherDecode ea_str :: Either String ExternalAttribution) of
                 Right ea' -> return ea'
                 Left err  -> fail err
             ea `shouldBe` expected_ea
           it "parsing should be successful" $ do
             potentialError `shouldBe` Nothing
           it "num of resources should match" $ do
             countFiles (_resources opossum) `shouldBe` 58805
           it "num of externalAttributions should match" $ do
             length (_externalAttributions opossum) `shouldBe` 13798
           it "num of resourcesToAttributions should match" $ do
             length (_resourcesToAttributions opossum) `shouldBe` 36931
           it "num of frequentLicenses should match" $ do
             length (_frequentLicenses opossum) `shouldBe` 427
  describe "Opossum Utils" $
    let source = ExternalAttribution_Source "test" 100
        expected_coordinates version =
          Coordinates Nothing Nothing (Just "name") (Just version) Nothing
        ea1 =
          ExternalAttribution
            source
            100
            Nothing
            Nothing
            (expected_coordinates "1.2")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            mempty
        ea2 =
          ExternalAttribution
            source
            100
            Nothing
            Nothing
            (expected_coordinates "1.3")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            mempty
        ea3 =
          ExternalAttribution
            source
            100
            Nothing
            Nothing
            (expected_coordinates "1.2")
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            justPreselectedFlags
     in do it "mergifyEA" $ do
             (ea1 `mergifyEA` ea1) `shouldBe` (Just ea1)
             (ea1 `mergifyEA` ea2) `shouldBe` Nothing
             (ea1 `mergifyEA` ea3) `shouldBe` (Just ea3)
  describe "Opossum Utils SPDX Converter with yml" $ do
    let oposum_from_spdx_yml =
          case (Y.decodeEither' (B.toStrict spdxYamlFileBS) :: Either Y.ParseException SPDXDocument) of
            Right spdxFile -> spdxToOpossum spdxFile
            Left err       -> undefined
    it "num of resources from spdx should match" $ do
      countFiles (_resources oposum_from_spdx_yml) `shouldBe` 0
    it "num of externalAttributions from spdx should match" $ do
      length (_externalAttributions oposum_from_spdx_yml) `shouldBe` 1054
    it "num of resourcesToAttributions from spdx should match" $ do
      length (_resourcesToAttributions oposum_from_spdx_yml) `shouldBe` 595
    it "num of frequentLicenses should from spdx match" $ do
      length (_frequentLicenses oposum_from_spdx_yml) `shouldBe` 0
    let (ExternalAttributionSources eas) =
          _externalAttributionSources oposum_from_spdx_yml
    it "externalAttributionSources sohuld be witten" $ do
      (Map.keys eas) `shouldBe` ["SPDXPackage"]
  describe "Opossum Utils SPDX Converter with JSON" $ do
    let oposum_from_spdx_json =
          case (A.eitherDecode spdxJsonFileBS :: Either String SPDXDocument) of
            Right spdxFile -> spdxToOpossum spdxFile
            Left err       -> undefined
    it "num of resources from spdx should match" $ do
      countFiles (_resources oposum_from_spdx_json) `shouldBe` 3
  describe "Opossum Utils SPDX Converter with tern JSON" $ do
    let oposum_from_spdx_json =
          case (A.eitherDecode spdxTernJsonFileBS :: Either String SPDXDocument) of
            Right spdxFile -> spdxToOpossum spdxFile
            Left err       -> undefined
    it "num of resources from spdx should match" $ do
      writeOpossumStats oposum_from_spdx_json
      length (_externalAttributions oposum_from_spdx_json) `shouldBe` 120
      countFiles (_resources oposum_from_spdx_json) `shouldBe` 0
  describe "Opossum Utils ScanCode Converter" $ do
    it "should render licenses for packages correctly" $ do
      let package = ScancodePackage { _scp_purl = Nothing
                                    , _scp_licenses = "MIT AND Apache-2.0"
                                    , _scp_copyright = Nothing
                                    , _scp_dependencies = []
                                    }
          eaFromPackage = fromJust $ scancodePackageToEA package
          licenseStringInEA = (fromJust . _licenseName) eaFromPackage
      licenseStringInEA `shouldBe` "MIT AND Apache-2.0"
    it "should render licenses for FileEntries correctly" $ do
      let file = ScancodeFileEntry { _scfe_file = "/not/existing/path"
                                   , _scfe_is_file = True
                                   , _scfe_license = "MIT AND Apache-2.0"
                                   , _scfe_copyrights = []
                                   , _scfe_packages = []
                                   }
          eaFromFile = fromJust $ scancodeFileEntryToEA file 
          licenseStringInEA = (fromJust . _licenseName) eaFromFile
      licenseStringInEA `shouldBe` "MIT AND Apache-2.0"
    it "should parse json file" $ do
      let decoded =
            (A.eitherDecode scancodeJsonBS :: Either String ScancodeFile)
          files = (_scf_files . (fromRight (ScancodeFile (Y.Null) []))) decoded
          pomFile = head $ filter (\f -> _scfe_file f == "pom.xml") files
          mslFile =
            head $
            filter
              (\f ->
                 _scfe_file f ==
                 "src/main/java/org/spdx/tools/MatchingStandardLicenses.java")
              files
      (isRight decoded) `shouldBe` True
      _scfe_file pomFile `shouldBe` "pom.xml"
      _scfe_copyrights pomFile `shouldBe`
        ["Copyright (c) 2020 Source Auditor Inc. Gary O'Neall"]
    opossumFromSC <- runIO $ parseScancodeBS scancodeJsonBS
    it "should parse json to opossum" $ do
      countFiles (_resources opossumFromSC) `shouldBe` 105
    let (ExternalAttributionSources eas) =
          _externalAttributionSources opossumFromSC
    it "externalAttributionSources sohuld be witten" $ do
      (List.sort $ Map.keys eas) `shouldBe` ["Scancode", "Scancode-Package"]
  describe "Opossum Utils Scanpipe Converter" $ do
    let decoded = (A.eitherDecode scanpipeJsonBS :: Either String ScanpipeFile)
        files = (_spf_files . fromRight undefined) decoded
        layers = (_spf_layers . fromRight undefined) decoded
        err =
          case decoded of
            Left err' -> err'
            Right _   -> ""
    it "should parse json file" $ do
      err `shouldBe` ""
      isRight decoded `shouldBe` True
    it "should contain layer information" $ do length layers `shouldBe` 10
    opossumFromSC <- runIO $ parseScanpipeBS scanpipeJsonBS
    it "should parse json to opossum" $ do
      countFiles (_resources opossumFromSC) `shouldBe` 12690
  describe "Opossum Utils Dependency-Check Converter" $ do
    it "should parse json to Package" $ do
      let jsonStr = "{ \"id\": \"pkg:nuget\\/Antlr4BuildTasks@8.13\" }"
      let decoded =
            (A.eitherDecode jsonStr :: Either String DependencyCheckPackage)
      (isRight decoded) `shouldBe` True
    it "should parse json file" $ do
      let decoded =
            (A.eitherDecode dependencyCheckJsonBS :: Either String DependencyCheckFile)
      (isRight decoded) `shouldBe` True
    opossumFromDC <- runIO $ parseDependencyCheckBS dependencyCheckJsonBS
    it "should parse json to opossum" $ do
      countFiles (_resources opossumFromDC) `shouldBe` 62
  describe "Opossum Utils Scanoss Parser" $ do
    it "should parse json file" $ do
      let decoded =
            (A.eitherDecode scanossJsonBS :: Either String (Map.Map String ScanossFindings))
      (isRight decoded) `shouldBe` True
    opossumFromSCA <- runIO $ parseScanossBS scanossJsonBS
    it "should parse json to opossum" $ do
      countFiles (_resources opossumFromSCA) `shouldBe` 183
  describe "Opossum Utils Opossum File Parser" $ do
    it "should parse .opossum file to opossum" $ do
      dotOpossumInputResult <- parseOpossum dotOpossumFilePath
      let resultProjectId = _metadata dotOpossumInputResult Map.! "projectId"
      let resultFirstResourceToAttributions = toString (head (_resourcesToAttributions dotOpossumInputResult Map.! "/src/index.html"))
      resultProjectId `shouldBe` "12345-42"
      resultFirstResourceToAttributions `shouldBe` "578b448b-2ba8-4285-810f-ce7b11ac260c"
      countFiles (_resources dotOpossumInputResult) `shouldBe` 3

main :: IO ()
main = hspec $ opossumSpec
