{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDX.PackageInformation
  where

import YACP.Core
import YACP.SPDX.Common
import YACP.ParserHelper

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

data SPDXPackageVerificationCode
--               "packageVerificationCode" : {
--                 "type" : "object",
--                 "properties" : {
--                   "packageVerificationCodeValue" : {
--                     "description" : "The actual package verification code as a hex encoded value.",
--                     "type" : "string"
--                   },
--                   "packageVerificationCodeExcludedFiles" : {
--                     "description" : "A file that was excluded when calculating the package verification code. This is usually a file containing SPDX data regarding the package. If a package contains more than one SPDX file all SPDX files must be excluded from the package verification code. If this is not done it would be impossible to correctly calculate the verification codes in both files.",
--                     "type" : "array",
--                     "items" : {
--                       "description" : "A file that was excluded when calculating the package verification code. This is usually a file containing SPDX data regarding the package. If a package contains more than one SPDX file all SPDX files must be excluded from the package verification code. If this is not done it would be impossible to correctly calculate the verification codes in both files.",
--                       "type" : "string"
--                     }
--                   }
--                 },
--                 "description" : "A manifest based verification code (the algorithm is defined in section 4.7 of the full specification) of the SPDX Item. This allows consumers of this data and/or database to determine if an SPDX item they have in hand is identical to the SPDX item from which the data was produced. This algorithm works even if the SPDX document is included in the SPDX item."
--               },
  = SPDXPackageVerificationCode
  { _SPDXPackageVerificationCode_packageVerificationCodeValue :: String
  , _SPDXPackageVerificationCode_packageVerificationCodeExcludedFiles :: [String]
  } deriving (Eq, Show)
instance A.FromJSON SPDXPackageVerificationCode where
  parseJSON = A.withObject "SPDXPackageVerificationCode" $ \v ->
    SPDXPackageVerificationCode
    <$> v A..: "packageVerificationCodeValue"
    <*> v .:?! "packageVerificationCodeExcludedFiles"

data SPDXPackage
--         "packages" : {
--           "description" : "Packages referenced in the SPDX document",
--           "type" : "array",
--           "items" : {
--             "type" : "object",
--             "properties" : {
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
--               "supplier" : {
--                 "description" : "The name and, optionally, contact information of the person or organization who was the immediate supplier of this package to the recipient. The supplier may be different than originator when the software has been repackaged. Values of this property must conform to the agent and tool syntax.",
--                 "type" : "string"
--               },
--               "homepage" : {
--                 "type" : "string"
--               },
--               "packageVerificationCode" : {
--                 "type" : "object",
--                 "properties" : {
--                   "packageVerificationCodeValue" : {
--                     "description" : "The actual package verification code as a hex encoded value.",
--                     "type" : "string"
--                   },
--                   "packageVerificationCodeExcludedFiles" : {
--                     "description" : "A file that was excluded when calculating the package verification code. This is usually a file containing SPDX data regarding the package. If a package contains more than one SPDX file all SPDX files must be excluded from the package verification code. If this is not done it would be impossible to correctly calculate the verification codes in both files.",
--                     "type" : "array",
--                     "items" : {
--                       "description" : "A file that was excluded when calculating the package verification code. This is usually a file containing SPDX data regarding the package. If a package contains more than one SPDX file all SPDX files must be excluded from the package verification code. If this is not done it would be impossible to correctly calculate the verification codes in both files.",
--                       "type" : "string"
--                     }
--                   }
--                 },
--                 "description" : "A manifest based verification code (the algorithm is defined in section 4.7 of the full specification) of the SPDX Item. This allows consumers of this data and/or database to determine if an SPDX item they have in hand is identical to the SPDX item from which the data was produced. This algorithm works even if the SPDX document is included in the SPDX item."
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
--                 }
--               },
--               "downloadLocation" : {
--                 "description" : "The URI at which this package is available for download. Private (i.e., not publicly reachable) URIs are acceptable as values of this property. The values http://spdx.org/rdf/terms#none and http://spdx.org/rdf/terms#noassertion may be used to specify that the package is not downloadable or that no attempt was made to determine its download location, respectively.",
--                 "type" : "string"
--               },
--               "filesAnalyzed" : {
--                 "description" : "Indicates whether the file content of this package has been available for or subjected to analysis when creating the SPDX document. If false indicates packages that represent metadata or URI references to a project, product, artifact, distribution or a component. If set to false, the package must not contain any files.",
--                 "type" : "boolean"
--               },
--               "externalRefs" : {
--                 "description" : "An External Reference allows a Package to reference an external source of additional information, metadata, enumerations, asset identifiers, or downloadable content believed to be relevant to the Package.",
--                 "type" : "array",
--                 "items" : {
--                   "type" : "object",
--                   "properties" : {
--                     "comment" : {
--                       "type" : "string"
--                     },
--                     "referenceCategory" : {
--                       "description" : "Category for the external reference",
--                       "type" : "string",
--                       "enum" : [ "OTHER", "SECURITY", "PACKAGE_MANAGER" ]
--                     },
--                     "referenceLocator" : {
--                       "description" : "The unique string with no spaces necessary to access the package-specific information, metadata, or content within the target location. The format of the locator is subject to constraints defined by the <type>.",
--                       "type" : "string"
--                     },
--                     "referenceType" : {
--                       "description" : "Type of the external reference. These are definined in an appendix in the SPDX specification.",
--                       "type" : "string"
--                     }
--                   },
--                   "description" : "An External Reference allows a Package to reference an external source of additional information, metadata, enumerations, asset identifiers, or downloadable content believed to be relevant to the Package."
--                 }
--               },
--               "licenseComments" : {
--                 "description" : "The licenseComments property allows the preparer of the SPDX document to describe why the licensing in spdx:licenseConcluded was chosen.",
--                 "type" : "string"
--               },
--               "name" : {
--                 "description" : "Identify name of this SpdxElement.",
--                 "type" : "string"
--               },
--               "hasFiles" : {
--                 "description" : "Indicates that a particular file belongs to a package.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "SPDX ID for File.  Indicates that a particular file belongs to a package.",
--                   "type" : "string"
--                 }
--               },
--               "comment" : {
--                 "type" : "string"
--               },
--               "summary" : {
--                 "description" : "Provides a short description of the package.",
--                 "type" : "string"
--               },
--               "copyrightText" : {
--                 "description" : "The text of copyright declarations recited in the Package or File.",
--                 "type" : "string"
--               },
--               "originator" : {
--                 "description" : "The name and, optionally, contact information of the person or organization that originally created the package. Values of this property must conform to the agent and tool syntax.",
--                 "type" : "string"
--               },
--               "packageFileName" : {
--                 "description" : "The base name of the package file name. For example, zlib-1.2.5.tar.gz.",
--                 "type" : "string"
--               },
--               "licenseInfoFromFiles" : {
--                 "description" : "The licensing information that was discovered directly within the package. There will be an instance of this property for each distinct value of alllicenseInfoInFile properties of all files contained in the package.",
--                 "type" : "array",
--                 "items" : {
--                   "description" : "License expression for licenseInfoFromFiles.  The licensing information that was discovered directly within the package. There will be an instance of this property for each distinct value of alllicenseInfoInFile properties of all files contained in the package.",
--                   "type" : "string"
--                 }
--               },
--               "versionInfo" : {
--                 "description" : "Provides an indication of the version of the package that is described by this SpdxDocument.",
--                 "type" : "string"
--               },
--               "sourceInfo" : {
--                 "description" : "Allows the producer(s) of the SPDX document to describe how the package was acquired and/or changed from the original source.",
--                 "type" : "string"
--               },
--               "description" : {
--                 "description" : "Provides a detailed description of the package.",
--                 "type" : "string"
--               }
--             }
--           }
--         },
  = SPDXPackage
  { _SPDXPackage_SPDXID :: SPDXID
  , _SPDXPackage_raw :: A.Object
  , _SPDXPackage_name :: String
  , _SPDXPackage_versionInfo :: Maybe String
  , _SPDXPackage_packageFileName :: Maybe String
  , _SPDXPackage_supplier :: Maybe String
  , _SPDXPackage_originator :: Maybe String
  , _SPDXPackage_downloadLocation :: SPDXMaybe String
  , _SPDXPackage_filesAnalyzed :: Bool
  , _SPDXPackage_packageVerificationCode :: Maybe SPDXPackageVerificationCode
  , _SPDXPackage_checksums :: [SPDXChecksum]
  , _SPDXPackage_homepage :: Maybe String
  , _SPDXPackage_sourceInfo :: Maybe String
  , _SPDXPackage_licenseConcluded :: SPDXMaybe SPDX.LicenseExpression
  , _SPDXPackage_licenseInfoFromFiles :: [String]
  , _SPDXPackage_licenseDeclared :: SPDXMaybe SPDX.LicenseExpression
  , _SPDXPackage_licenseComments :: Maybe String
  , _SPDXPackage_copyrightText :: SPDXMaybe String
  , _SPDXPackage_summary :: Maybe String
  , _SPDXPackage_description :: Maybe String
  , _SPDXPackage_comment :: Maybe String
  -- , _SPDXPackage_externalRefs :: [SPDXEternalRef]
  , _SPDXPackage_attributionTexts :: Maybe [String]
  -- , _SPDXPackage_annotations :: SPDXAnnotations
  , _SPDXPackage_hasFiles :: Maybe [SPDXID]
  } deriving (Eq, Show)
instance A.FromJSON SPDXPackage where
  parseJSON = A.withObject "SPDXPackage" $ \v ->
    SPDXPackage
    <$> v A..: "SPDXID" -- 3.2
    <*> pure v
    <*> v A..: "name" -- 3.1
    <*> v A..:? "versionInfo" -- 3.3
    <*> v A..:? "packageFileName" -- 3.4
    <*> v A..:? "supplier" -- 3.5
    <*> v A..:? "originator" -- 3.6
    <*> v A..: "downloadLocation" -- 3.7
    <*> fmap (\case
                 Nothing -> True
                 Just b  -> b) (v A..:? "filesAnalyzed") -- 3.8
    <*> v A..:? "packageVerificationCode" -- 3.9
    <*> fmap (\case
                 Nothing -> []
                 Just cs -> cs) (v A..:? "checksums") -- 3.10
    <*> v A..:? "homepage" -- 3.11
    <*> v A..:? "sourceInfo" -- 3.12
    <*> fmap parseLicenseExpression (v A..: "licenseConcluded") -- 3.13 -- not in schema
    <*> fmap (\case
                 Nothing -> []
                 Just cs -> cs) (v A..:? "licenseInfoFromFiles") -- 3.14
    <*> fmap parseLicenseExpression (v A..: "licenseDeclared") -- 3.15 -- not in schema
    <*> v A..:? "licenseComments" -- 3.16
    <*> fmap (\case
                 Nothing -> NONE
                 Just ct -> ct) (v A..:? "copyrightText") -- 3.17
    <*> v A..:? "summary" -- 3.18
    <*> v A..:? "description" -- 3.19
    <*> v A..:? "comment" -- 3.20
    -- <*> v A..: "externalRefs" -- 3.21
    -- External Reference Comment <a name="3.22"></a>
    <*> v A..:? "attributionTexts" -- 3.23
    -- <*> v A..: "annotations"
    <*> v A..:? "hasFiles"
