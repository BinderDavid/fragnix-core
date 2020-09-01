{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Fragnix.Core.Slice
  ( SliceID
  , Slice(..)
  , Use(..)
  , Reference(..)
  , OriginalModule
  , Language(..)
  , Fragment(..)
  , Qualification
  , Name(..)
  , UsedName(..)
  , Instance(..)
  , InstancePart(..)
  , InstanceID
  , sliceIDModuleName
  , moduleNameReference
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Char (isDigit)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as Text (unpack,pack)
import GHC.Generics (Generic)

-- Slice

data Slice = Slice SliceID Language Fragment [Use] [Instance]

deriving instance Show Slice
deriving instance Eq Slice
deriving instance Ord Slice
deriving instance Generic Slice
instance ToJSON Slice
instance FromJSON Slice

-- Language

data Language = Language [GHCExtension]

deriving instance Show Language
deriving instance Eq Language
deriving instance Ord Language
deriving instance Generic Language
instance ToJSON Language
instance FromJSON Language
instance Hashable Language

-- Fragment

data Fragment = Fragment [SourceCode]

deriving instance Show Fragment
deriving instance Eq Fragment
deriving instance Ord Fragment
deriving instance Generic Fragment
instance ToJSON Fragment
instance FromJSON Fragment
instance Hashable Fragment

-- Use

data Use = Use (Maybe Qualification) UsedName Reference

deriving instance Show Use
deriving instance Eq Use
deriving instance Ord Use
deriving instance Generic Use
instance ToJSON Use
instance FromJSON Use
instance Hashable Use

-- Instance

data Instance = Instance InstancePart InstanceID

deriving instance Show Instance
deriving instance Eq Instance
deriving instance Ord Instance
deriving instance Generic Instance
instance ToJSON Instance
instance FromJSON Instance
instance Hashable Instance

-- InstancePart

data InstancePart
  = OfThisClass
  | OfThisClassForUnknownType
  | ForThisType
  | ForThisTypeOfUnknownClass

deriving instance Show InstancePart
deriving instance Eq InstancePart
deriving instance Ord InstancePart
deriving instance Generic InstancePart
instance ToJSON InstancePart
instance FromJSON InstancePart
instance Hashable InstancePart

type InstanceID = SliceID

-- Reference

data Reference
  = OtherSlice SliceID
  | Builtin OriginalModule

deriving instance Show Reference
deriving instance Eq Reference
deriving instance Ord Reference
deriving instance Generic Reference
instance ToJSON Reference
instance FromJSON Reference
instance Hashable Reference

-- UsedName

data UsedName
  = ValueName Name
  | TypeName Name
  | ConstructorName TypeName Name

deriving instance Show UsedName
deriving instance Eq UsedName
deriving instance Ord UsedName
deriving instance Generic UsedName
instance ToJSON UsedName
instance FromJSON UsedName
instance Hashable UsedName

-- Name

data Name
  = Identifier Text
  | Operator Text

deriving instance Show Name
deriving instance Eq Name
deriving instance Ord Name
deriving instance Generic Name
instance ToJSON Name
instance FromJSON Name
instance Hashable Name

type TypeName = Name

type SliceID = Text
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text
type GHCExtension = Text


-- Auxilliary Functions

-- | The name we give to the module generated for a slice with the given ID.
sliceIDModuleName :: SliceID -> String
sliceIDModuleName sliceID = "F" ++ Text.unpack sliceID

-- | We abuse module names to either refer to builtin modules or to a slice.
-- If the module name refers to a slice it starts with F followed by
-- digits.
moduleNameReference :: String -> Reference
moduleNameReference moduleName =
  case moduleName of
    ('F':rest)
      | all isDigit rest -> OtherSlice (Text.pack rest)
      | otherwise -> Builtin (Text.pack moduleName)
    _ -> Builtin (Text.pack moduleName)
