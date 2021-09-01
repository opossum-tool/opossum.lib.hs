{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDX.DocumentCreationInformation
  where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import YACP.SPDX.Common

data SPDXCreationInfo
  = SPDXCreationInfo -- ^One instance is required for each SPDX file produced. It provides the necessary information for forward and backward compatibility for processing tools.
  { _SPDXCreationInfo_comment :: Maybe String
  , _SPDXCreationInfo_created :: String -- ^Identify when the SPDX file was originally created. The date is to be specified according to combined date and time in UTC format as specified in ISO 8601 standard. This field is distinct from the fields in section 8, which involves the addition of information during a subsequent review.
  , _SPDXCreationInfo_creators :: [String] -- ^Identify who (or what, in the case of a tool) created the SPDX file. If the SPDX file was created by an individual, indicate the person's name. If the SPDX file was created on behalf of a company or organization, indicate the entity name. If the SPDX file was created using a software tool, indicate the name and version for that tool. If multiple participants or tools were involved, use multiple instances of this field. Person name or organization name may be designated as “anonymous” if appropriate.
  , _SPDXCreationInfo_licenseListVersion :: String -- ^An optional field for creators of the SPDX file to provide the version of the SPDX License List used when the SPDX file was created.
  } deriving (Eq, Show)
instance A.FromJSON SPDXCreationInfo where
  parseJSON = A.withObject "SPDXCreationInfo" $ \v -> let
    in SPDXCreationInfo
    <$> v A..:? "comment"
    <*> v A..: "created"
    <*> v A..: "creators"
    <*> v A..: "licenseListVersion"
