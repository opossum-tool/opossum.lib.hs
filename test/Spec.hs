-- SPDX-FileCopyrightText: Maximilian Huber
-- SPDX-FileCopyrightText: TNG Technology Consulting GmbH <https://www.tngtech.com>
--
-- SPDX-License-Identifier: BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Exception              ( evaluate )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as B
import           Data.Either
import           Data.FileEmbed                 ( embedFile )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Vector                   as V
import qualified Data.Yaml                     as Y
import           System.Exit
import           System.FilePath                ( (</>) )
import           Test.Hspec
import           Test.QuickCheck

import           SPDX.Document

import           Opossum.Opossum
import           Opossum.OpossumSPDXUtils
import           Opossum.OpossumUtils
import           Opossum.OpossumDependencyCheckUtils
import           Opossum.OpossumScancodeUtils
import Opossum.OpossumScanossUtils


opossumFileBS :: B.ByteString
opossumFileBS =
  B.fromStrict $(embedFile "test/data/zephyr-bundle.json.opossum.json")

spdxYamlFileBS :: B.ByteString
spdxYamlFileBS = B.fromStrict $(embedFile "test/data/document.spdx.yml")

scancodeJsonBS :: B.ByteString
scancodeJsonBS = B.fromStrict $(embedFile "test/data/tools-java.scancode.json")

dependencyCheckJsonBS :: B.ByteString
dependencyCheckJsonBS = B.fromStrict $(embedFile "test/data/dependency-check-report.json")

scanossCheckJsonBS :: B.ByteString
scanossCheckJsonBS = B.fromStrict $(embedFile "test/data/zlib_sca.json")

opossumSpec = do
  describe "Opossum Model"
    $ let
        exmpResourses =
          [ "root" </> "subfolder" </> "file1"
          , "root" </> "subfolder" </> "file2"
          , "root" </> "file3"
          ]
        parsedResources = Opossum_Resources
          (Map.singleton
            "root"
            (Opossum_Resources
              (Map.singleton
                "subfolder"
                (Opossum_Resources Map.empty (Set.fromList ["file1", "file2"]))
              )
              (Set.singleton "file3")
            )
          )
          Set.empty
        serializedResources = B.concat
          [ "{"
          , "\"root\":{"
          , "\"subfolder\":{"
          , "\"file1\":1,"
          , "\"file2\":1"
          , "},"
          , "\"file3\":1"
          , "}"
          , "}"
          ]
        otherResources = Opossum_Resources
          (Map.singleton
            "root"
            (Opossum_Resources
              (Map.singleton
                "other"
                (Opossum_Resources Map.empty (Set.singleton "file4"))
              )
              Set.empty
            )
          )
          (Set.singleton "file5")
        allResourcesSerialized
          = "{\"root\":{\"other\":{\"file4\":1},\"subfolder\":{\"file1\":1,\"file2\":1},\"file3\":1},\"file5\":1}"
      in
        do
          it "testFolderAndFileMerging" $ do
            (fpToResources True "path/to/file")
              <>         (fpToResources False "path/other/dir")
              `shouldBe` Opossum_Resources
                           (Map.singleton
                             "path"
                             (Opossum_Resources
                               (Map.fromList
                                 [ ( "to"
                                   , Opossum_Resources Map.empty
                                                       (Set.singleton "file")
                                   )
                                 , ( "other"
                                   , Opossum_Resources
                                     (Map.singleton "dir" mempty)
                                     Set.empty
                                   )
                                 ]
                               )
                               Set.empty
                             )
                           )
                           Set.empty
            countFiles
                (  (fpToResources True "path/to/file")
                <> (fpToResources False "path/other/dir")
                )
              `shouldBe` 1

          it "testFileListParsing" $ do
            fpsToResources exmpResourses `shouldBe` parsedResources
          it "testFileListSerialization" $ do
            A.encode parsedResources `shouldBe` serializedResources
          it "testFileListDeSerialization" $ do
            A.decode serializedResources `shouldBe` (Just parsedResources)
          it "testMergeAndFileListSerialization" $ do
            A.encode (parsedResources <> otherResources)
              `shouldBe` allResourcesSerialized

  describe "Opossum Model deserialization"
    $ let
        result         = (A.eitherDecode opossumFileBS :: Either String Opossum)
        potentialError = case result of
          Right _   -> Nothing
          Left  err -> Just err
        opossum = case result of
          Right opossum' -> opossum'
          Left  _        -> undefined
      in
        do

          it "parsing of EA works, case 1" $ do
            let
              ea_str = B.fromStrict $ C.unlines
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
              expected_source = Opossum_ExternalAttribution_Source "tern" 100
              expected_coordinates = Opossum_Coordinates Nothing
                                                         Nothing
                                                         (Just "adduser")
                                                         (Just "3.118")
                                                         Nothing
              expected_ea = Opossum_ExternalAttribution
                expected_source
                100
                Nothing
                Nothing
                expected_coordinates
                (Just "Some Copyright")
                (Just "MIT AND GPL-2.0-or-later")
                Nothing
                (Just "https://github.com/some/repo")
                mempty
            ea <-
              case
                (A.eitherDecode ea_str :: Either
                    String
                    Opossum_ExternalAttribution
                )
              of
                Right ea' -> return ea'
                Left  err -> fail err
            ea `shouldBe` expected_ea

          it "parsing of EA works, case 2" $ do
            let
              ea_str = B.fromStrict $ C.unlines
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
              expected_source = Opossum_ExternalAttribution_Source "tern" 100
              expected_coordinates = Opossum_Coordinates Nothing
                                                         Nothing
                                                         (Just "adduser")
                                                         (Just "3.118")
                                                         Nothing
              expected_ea = Opossum_ExternalAttribution
                expected_source
                100
                Nothing
                Nothing
                expected_coordinates
                (Just "Some Copyright")
                Nothing
                Nothing
                Nothing
                justPreselectedFlags
            ea <-
              case
                (A.eitherDecode ea_str :: Either
                    String
                    Opossum_ExternalAttribution
                )
              of
                Right ea' -> return ea'
                Left  err -> fail err
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

  describe "Opossum Utils"
    $ let source = Opossum_ExternalAttribution_Source "test" 100
          expected_coordinates version =
            Opossum_Coordinates Nothing Nothing (Just "name") (Just version) Nothing
          ea1 = Opossum_ExternalAttribution source
                                            100
                                            Nothing
                                            Nothing
                                            (expected_coordinates "1.2")
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                            mempty
          ea2 = Opossum_ExternalAttribution source
                                            100
                                            Nothing
                                            Nothing
                                            (expected_coordinates "1.3")
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                            mempty
          ea3 = Opossum_ExternalAttribution source
                                            100
                                            Nothing
                                            Nothing
                                            (expected_coordinates "1.2")
                                            Nothing
                                            Nothing
                                            Nothing
                                            Nothing
                                            justPreselectedFlags
      in  do
            it "mergifyEA" $ do
              (ea1 `mergifyEA` ea1) `shouldBe` (Just ea1)
              (ea1 `mergifyEA` ea2) `shouldBe` Nothing
              (ea1 `mergifyEA` ea3) `shouldBe` (Just ea3)

  describe "Opossum Utils SPDX Converter" $ do
    opossum_from_ort <-
      runIO
        $ case
            (Y.decodeEither' (B.toStrict spdxYamlFileBS) :: Either
                Y.ParseException
                SPDXDocument
            )
          of
            Right spdxFile -> spdxToOpossum spdxFile
            Left  err      -> fail (show err)

    it "num of resources from spdx should match" $ do
      countFiles (_resources opossum_from_ort) `shouldBe` 0
    it "num of externalAttributions from spdx should match" $ do
      length (_externalAttributions opossum_from_ort) `shouldBe` 352
    it "num of resourcesToAttributions from spdx should match" $ do
      length (_resourcesToAttributions opossum_from_ort) `shouldBe` 298
    it "num of frequentLicenses should from spdx match" $ do
      length (_frequentLicenses opossum_from_ort) `shouldBe` 0

  describe "Opossum Utils ScanCode Converter" $ do
    it "should parse json file" $ do
      let decoded = (A.eitherDecode scancodeJsonBS :: Either String ScancodeFile)
          files = (_scf_files . (fromRight (ScancodeFile (Y.Null) []))) decoded
          pomFile = head $ filter (\f -> _scfe_file f == "pom.xml") files
      (isRight decoded) `shouldBe` True
      _scfe_file pomFile `shouldBe` "pom.xml"
      _scfe_copyrights pomFile `shouldBe` [ "Copyright (c) 2020 Source Auditor Inc. Gary O'Neall" ]

    opossumFromSC <- runIO $ parseScancodeBS scancodeJsonBS
    it "should parse json to opossum" $ do
      countFiles (_resources opossumFromSC) `shouldBe` 105

  describe "Opossum Utils Dependency-Check Converter" $ do

    it "should parse json to Package" $ do
      let jsonStr = "{ \"id\": \"pkg:nuget\\/Antlr4BuildTasks@8.13\" }"
      let decoded = (A.eitherDecode jsonStr :: Either String DependencyCheckPackage)
      (isRight decoded) `shouldBe` True

    it "should parse json file" $ do
      let decoded = (A.eitherDecode dependencyCheckJsonBS :: Either String DependencyCheckFile)
      (isRight decoded) `shouldBe` True

    opossumFromDC <- runIO $ parseDependencyCheckBS dependencyCheckJsonBS
    it "should parse json to opossum" $ do
      countFiles (_resources opossumFromDC) `shouldBe` 62
  
  describe "Opossum Utils Scanoss Parser" $ do
    it "should parse json file" $ do
      let decoded = (A.eitherDecode scanossCheckJsonBS :: Either String (Map.Map String ScanossFindings))
      (isRight decoded) `shouldBe` True

main :: IO ()
main = hspec $ opossumSpec
