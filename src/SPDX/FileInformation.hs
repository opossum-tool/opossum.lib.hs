{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDX.FileInformation
  where

import YACP.Core
import YACP.SPDX.Common

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

data SPDXFileType
  = OTHER
  | DOCUMENTATION
  | IMAGE
  | VIDEO
  | ARCHIVE
  | SPDX
  | APPLICATION
  | SOURCE
  | BINARY
  | TEXT
  | AUDIO
  deriving (Eq, Show, Generic)
instance A.FromJSON SPDXFileType
data SPDXFile
--         "files" : {
--           "description" : "Files referenced in the SPDX document",
--           "type" : "array",
--           "items" : {
--             "type" : "object",
--             "properties" : {
--               "fileTypes" : {
--                 "description" : "The type of the file.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "The type of the file.",
--                   "type" : "string",
--                   "enum" : [ "OTHER", "DOCUMENTATION", "IMAGE", "VIDEO", "ARCHIVE", "SPDX", "APPLICATION", "SOURCE", "BINARY", "TEXT", "AUDIO" ]
--                 }
--               },
--               "attributionTexts" : {
--                 "description" : "This field provides a place for the SPDX data creator to record acknowledgements that may be required to be communicated in some contexts. This is not meant to include theactual complete license text (see licenseConculded and licenseDeclared), and may or may not include copyright notices (see also copyrightText). The SPDX data creator may use this field to record other acknowledgements, such as particular clauses from license texts, which may be necessary or desirable to reproduce.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "This field provides a place for the SPDX data creator to record acknowledgements that may be required to be communicated in some contexts. This is not meant to include theactual complete license text (see licenseConculded and licenseDeclared), and may or may not include copyright notices (see also copyrightText). The SPDX data creator may use this field to record other acknowledgements, such as particular clauses from license texts, which may be necessary or desirable to reproduce.",
--                   "type" : "string"
--                 }
--               },
--               "annotations" : {
--                 "description" : "Provide additional information about an SpdxElement.",
--                 "type" : "array",
--                 "items" : {
--                   "type" : "object",
--                   "properties" : {
--                     "annotationDate" : {
--                       "description" : "Identify when the comment was made. This is to be specified according to the combined date and time in the UTC format, as specified in the ISO 8601 standard.",
--                       "type" : "string"
--                     },
--                     "comment" : {
--                       "type" : "string"
--                     },
--                     "annotator" : {
--                       "description" : "This field identifies the person, organization or tool that has commented on a file, package, or the entire document.",
--                       "type" : "string"
--                     },
--                     "annotationType" : {
--                       "description" : "Type of the annotation.",
--                       "type" : "string",
--                       "enum" : [ "OTHER", "REVIEW" ]
--                     }
--                   },
--                   "description" : "An Annotation is a comment on an SpdxItem by an agent."
--                 }
--               },
--               "checksums" : {
--                 "description" : "The checksum property provides a mechanism that can be used to verify that the contents of a File or Package have not changed.",
--                 "type" : "array",
--                 "items" : {
--                   "type" : "object",
--                   "properties" : {
--                     "algorithm" : {
--                       "description" : "Identifies the algorithm used to produce the subject Checksum. Currently, SHA-1 is the only supported algorithm. It is anticipated that other algorithms will be supported at a later time.",
--                       "type" : "string",
--                       "enum" : [ "SHA256", "SHA1", "SHA384", "MD2", "MD4", "SHA512", "MD6", "MD5", "SHA224" ]
--                     },
--                     "checksumValue" : {
--                       "description" : "The checksumValue property provides a lower case hexidecimal encoded digest value produced using a specific algorithm.",
--                       "type" : "string"
--                     }
--                   },
--                   "description" : "A Checksum is value that allows the contents of a file to be authenticated. Even small changes to the content of the file will change its checksum. This class allows the results of a variety of checksum and cryptographic message digest algorithms to be represented."
--                 },
--                 "minItems" : 1
--               },
--               "noticeText" : {
--                 "description" : "This field provides a place for the SPDX file creator to record potential legal notices found in the file. This may or may not include copyright statements.",
--                 "type" : "string"
--               },
--               "artifactOfs" : {
--                 "description" : "Indicates the project in which the SpdxElement originated. Tools must preserve doap:homepage and doap:name properties and the URI (if one is known) of doap:Project resources that are values of this property. All other properties of doap:Projects are not directly supported by SPDX and may be dropped when translating to or from some SPDX formats.",
--                 "type" : "array",
--                 "items" : {
--                   "type" : "object",
--                   "properties" : { }
--                 }
--               },
--               "licenseComments" : {
--                 "description" : "The licenseComments property allows the preparer of the SPDX document to describe why the licensing in spdx:licenseConcluded was chosen.",
--                 "type" : "string"
--               },
--               "fileName" : {
--                 "description" : "The name of the file relative to the root of the package.",
--                 "type" : "string"
--               },
--               "name" : {
--                 "description" : "Identify name of this SpdxElement.",
--                 "type" : "string"
--               },
--               "comment" : {
--                 "type" : "string"
--               },
--               "copyrightText" : {
--                 "description" : "The text of copyright declarations recited in the Package or File.",
--                 "type" : "string"
--               },
--               "fileContributors" : {
--                 "description" : "This field provides a place for the SPDX file creator to record file contributors. Contributors could include names of copyright holders and/or authors who may not be copyright holders yet contributed to the file content.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "This field provides a place for the SPDX file creator to record file contributors. Contributors could include names of copyright holders and/or authors who may not be copyright holders yet contributed to the file content.",
--                   "type" : "string"
--                 }
--               },
--               "licenseInfoInFiles" : {
--                 "description" : "Licensing information that was discovered directly in the subject file. This is also considered a declared license for the file.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "License expression for licenseInfoInFile.  Licensing information that was discovered directly in the subject file. This is also considered a declared license for the file.",
--                   "type" : "string"
--                 },
--                 "minItems" : 1
--               },
--               "licenseInfoFromFiles" : {
--                 "description" : "The licensing information that was discovered directly within the package. There will be an instance of this property for each distinct value of alllicenseInfoInFile properties of all files contained in the package.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "License expression for licenseInfoFromFiles.  The licensing information that was discovered directly within the package. There will be an instance of this property for each distinct value of alllicenseInfoInFile properties of all files contained in the package.",
--                   "type" : "string"
--                 }
--               },
--               "fileDependencies" : {
--                 "type" : "array",
--                 "items" : {
--                   "description" : "SPDX ID for File",
--                   "type" : "string"
--                 }
--               }
--             }
--           }
--         },
  = SPDXFile
  { _SPDXFile_SPDXID :: SPDXID
  , _SPDXFile_raw :: A.Object
  , _SPDXFile_fileName :: String
  , _SPDXFile_fileTypes :: Maybe [SPDXFileType]
  , _SPDXFile_checksums :: [SPDXChecksum]
  , _SPDXFile_LicenseConcluded :: SPDXMaybe SPDX.LicenseExpression
  , _SPDXFile_licenseInfoInFiles :: [String]
  , _SPDXFile_licenseInfoFromFiles :: Maybe [String]
  , _SPDXFile_licenseComments :: Maybe String
  , _SPDXFile_copyrightText :: String
  , _SPDXFile_comment :: Maybe String
  , _SPDXFile_noticeText :: Maybe String
  , _SPDXFile_fileContributors :: Maybe [String]
  , _SPDXFile_attributionTexts :: Maybe [String]

  , _SPDXFile_fileDependencies :: Maybe [SPDXID]
  -- , _SPDXFile_annotations ::
  -- , _SPDXFile_artifactOfs ::???
  , _SPDXFile_name :: Maybe String
  } deriving (Eq, Show)
instance A.FromJSON SPDXFile where
  parseJSON = A.withObject "SPDXFile" $ \v ->
    SPDXFile
    <$> v A..: "SPDXID" -- 4.2
    <*> pure v
    <*> v A..: "fileName" -- 4.1
    <*> v A..:? "fileTypes" -- 4.3
    <*> v A..: "checksums" -- 4.4
    <*> fmap parseLicenseExpression (v A..: "licenseConcluded") -- 4.5
    <*> v A..: "licenseInfoInFiles" -- 4.6
    <*> v A..:? "licenseInfoFromFiles" -- ?
    <*> v A..:? "licenseComments" -- 4.7
    <*> v A..: "copyrightText" -- 4.8
    <*> v A..:? "comment" -- 4.12
    <*> v A..:? "noticeText" -- 4.13
    <*> v A..:? "fileContributors" -- 4.14
    <*> v A..:? "attributionTexts" -- 4.15

    -- deprecated
    <*> v A..:? "fileDependencies" -- 4.16

    -- <*> v A..: "annotations"
    -- <*> v A..: "artifactOfs"
    <*> v A..:? "name"
