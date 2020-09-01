{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Fragnix.Core.Environment
  ( Symbol(..)
  , Environment
  , Name(..)
  , ModuleName(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data Entity
    = Value
      { symbolModule :: Reference
      , symbolName :: Name
      }
      -- ^ value or function
    | Method
      { symbolModule :: Reference
      , symbolName :: Name
      , className :: Name
      }
      -- ^ class method
    | Selector
      { symbolModule :: Reference
      , symbolName :: Name
      , typeName :: Name
      , constructors :: [Name]
      }
      -- ^ record field selector
    | Constructor
      { symbolModule :: Reference
      , symbolName :: Name
      , typeName :: Name
      }
      -- ^ data constructor
    | Type
      { symbolModule :: Reference
      , symbolName :: Name
      }
      -- ^ type synonym
    | Data
      { symbolModule :: Reference
      , symbolName :: Name
      }
      -- ^ data type
    | NewType
      { symbolModule :: Reference
      , symbolName :: Name
      }
      -- ^ newtype
    | TypeFam
      { symbolModule :: Reference
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ type family
    | DataFam
      { symbolModule :: Reference
      , symbolName :: Name
      , associate :: Maybe Name
      }
      -- ^ data family
    | Class
      { symbolModule :: Reference
      , symbolName :: Name
      }
      -- ^ type class
    | PatternConstructor
      { symbolModule :: Reference
      , symbolName :: Name
      , patternTypeName :: Maybe Name
      }
      -- ^ pattern synonym constructor
    | PatternSelector
      { symbolModule :: Reference
      , symbolName :: Name
      , patternTypeName :: Maybe Name
      , patternConstructorName :: Name
      }

deriving instance Show Entity
deriving instance Eq Entity
deriving instance Ord Entity
deriving instance Generic Entity
instance ToJSON Entity
instance FromJSON Entity

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


-- | A map from module name to list of symbols it exports.
type Environment = Map ModuleName [Entity]
