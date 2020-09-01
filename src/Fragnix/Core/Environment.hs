{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Fragnix.Core.Environment where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Map (Map)
data Symbol
    = Value
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ value or function
    | Method
      { symbolModule :: ModuleName
      , symbolName :: Name
      , className :: Name
      }
      -- ^ class method
    | Selector
      { symbolModule :: ModuleName
      , symbolName :: Name
      , typeName :: Name
      , constructors :: [Name]
      }
      -- ^ record field selector
    | Constructor
      { symbolModule :: ModuleName
      , symbolName :: Name
      , typeName :: Name
      }
      -- ^ data constructor
    | Type
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ type synonym
    | Data
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ data type
    | NewType
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ newtype
    | TypeFam
      { symbolModule :: ModuleName
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ type family
    | DataFam
      { symbolModule :: ModuleName
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ data family
    | Class
      { symbolModule :: ModuleName
      , symbolName :: Name
      }
      -- ^ type class
    | PatternConstructor
      { symbolModule :: ModuleName
      , symbolName :: Name
      , patternTypeName :: Maybe Name
      }
      -- ^ pattern synonym constructor
    | PatternSelector
      { symbolModule :: ModuleName
      , symbolName :: Name
      , patternTypeName :: Maybe Name
      , patternConstructorName :: Name
      }

deriving instance Show Symbol
deriving instance Eq Symbol
deriving instance Ord Symbol
deriving instance Generic Symbol
instance ToJSON Symbol
instance FromJSON Symbol

-- | A map from module name to list of symbols it exports.
type Environment = Map ModuleName [Symbol]

-- | The name of a Haskell module.
data ModuleName = ModuleName Text

deriving instance Show ModuleName
deriving instance Eq ModuleName
deriving instance Ord ModuleName
deriving instance Generic ModuleName
instance ToJSON ModuleName
instance FromJSON ModuleName

-- | This type is used to represent variables, and also constructors.
data Name
    = Ident  Text   -- ^ /varid/ or /conid/.
    | Symbol Text   -- ^ /varsym/ or /consym/

deriving instance Show Name
deriving instance Eq Name
deriving instance Ord Name
deriving instance Generic Name
instance ToJSON Name
instance FromJSON Name
