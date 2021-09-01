{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDX.RelationshipsbetweenSPDXElements
  where

import YACP.Core
import YACP.SPDX.Common

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

data SPDXRelationship
--         "relationships" : {
--           "description" : "Relationships referenced in the SPDX document",
--           "type" : "array",
--           "items" : {
--             "type" : "object",
--             "properties" : {
--               "comment" : {
--                 "type" : "string"
--               },
--               "relationshipType" : {
--                 "description" : "Describes the type of relationship between two SPDX elements.",
--                 "type" : "string",
--                 "enum" : [ "VARIANT_OF", "COPY_OF", "PATCH_FOR", "TEST_DEPENDENCY_OF", "CONTAINED_BY", "DATA_FILE_OF", "OPTIONAL_COMPONENT_OF", "ANCESTOR_OF", "GENERATES", "CONTAINS", "OPTIONAL_DEPENDENCY_OF", "FILE_ADDED", "DEV_DEPENDENCY_OF", "DEPENDENCY_OF", "BUILD_DEPENDENCY_OF", "DESCRIBES", "PREREQUISITE_FOR", "HAS_PREREQUISITE", "PROVIDED_DEPENDENCY_OF", "DYNAMIC_LINK", "DESCRIBED_BY", "METAFILE_OF", "DEPENDENCY_MANIFEST_OF", "PATCH_APPLIED", "RUNTIME_DEPENDENCY_OF", "TEST_OF", "TEST_TOOL_OF", "DEPENDS_ON", "FILE_MODIFIED", "DISTRIBUTION_ARTIFACT", "DOCUMENTATION_OF", "GENERATED_FROM", "STATIC_LINK", "OTHER", "BUILD_TOOL_OF", "TEST_CASE_OF", "PACKAGE_OF", "DESCENDANT_OF", "FILE_DELETED", "EXPANDED_FROM_ARCHIVE", "DEV_TOOL_OF", "EXAMPLE_OF" ]
--               },
--               "relatedSpdxElement" : {
--                 "description" : "SPDX ID for SpdxElement.  A related SpdxElement.",
--                 "type" : "string"
--               }
--             }
--           }
--         }
  = SPDXRelationship
  { _SPDXRelationship_comment :: Maybe String
  , _SPDXRelationship_relationshipType :: RelationType
  , _SPDXRelationship_relatedSpdxElement :: SPDXID
  , _SPDXRelationship_spdxElementId :: SPDXID
  } deriving (Eq, Show)
instance A.FromJSON SPDXRelationship where
  parseJSON = A.withObject "SPDXRelationship" $ \v ->
    SPDXRelationship
    <$> v A..:? "comment"
    <*> v A..: "relationshipType"
    <*> v A..: "relatedSpdxElement"
    <*> v A..: "spdxElementId"
