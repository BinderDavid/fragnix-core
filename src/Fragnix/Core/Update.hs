{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Fragnix.Core.Update
  ( Update
  , UpdateID
  , PersistedUpdate(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Fragnix.Core.Slice (SliceID)

type Update = [(SliceID, SliceID)]

type UpdateID = Text

data PersistedUpdate = PersistedUpdate
  { updateID :: UpdateID
  , updateDescription :: Text
  , updateContent :: Update
  }

deriving instance Show PersistedUpdate
deriving instance Eq PersistedUpdate
deriving instance Generic PersistedUpdate
instance ToJSON PersistedUpdate
instance FromJSON PersistedUpdate

