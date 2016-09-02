{-|
Module      : Database.Persist.Internal.Parser.Types
Description : Persistent model file parsing functions
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta
-}

{-# LANGUAGE DeriveGeneric #-}

module Database.Persist.Internal.Parser.Types where

import           Data.Text (Text)
import           GHC.Generics

-- | A collection of data types with which you can recontruct a Persist Model file
-- | or create an altered version.
type ModelsFile = [ModelsFilePiece]

-- | Top level pieces of a Model file.
data ModelsFilePiece = ModelsFileEntity     Entity     |
                       ModelsFileComment    Comment    |
                       ModelsFileWhiteSpace WhiteSpace
  deriving (Eq,Read,Show,Generic)

-- | A single Persist Model Entity.
data Entity = Entity {
  entityName       :: Text
, entityDeriveJson :: Bool          -- | Person json
, entitySqlTable   :: Maybe Text    -- | Person sql=peoples
, entityChildren   :: [EntityChild]
} deriving (Eq,Read,Show,Generic)


-- | All of the child elements of a Persist Model Entity.
-- | They are all indented in the Model File.
data EntityChild = EntityChildEntityField   EntityField   |
                   EntityChildEntityUnique  EntityUnique  |
                   EntityChildEntityDerive  EntityDerive  |
                   EntityChildEntityPrimary EntityPrimary |
                   EntityChildEntityForeign EntityForeign |
                   EntityChildComment       Comment       |
                   EntityChildWhiteSpace    WhiteSpace
  deriving (Eq,Read,Show,Generic)

-- | A data row from an Entity.
data EntityField = EntityField {
  entityFieldName            :: Text
, entityFieldType            :: EntityFieldType
, entityFieldIsMigrationOnly :: Bool  -- | MigrationOnly
, entityFieldIsSafeToRemove  :: Bool  -- | SafeToRemove
, entityFieldDefault         :: Maybe Text -- | default=Nothing, default=now(), default=CURRENT_DATE
, entityFieldSqlRow          :: Maybe Text -- | sql=my_id_name
, entityFieldSqlType         :: Maybe Text -- | sqltype=varchar(255)
, entityFieldMaxLen          :: Maybe Int
}  deriving (Eq,Read,Show,Generic)


-- | Table rows can be strict or lazy
data Strictness
  -- | Persist Model types are strict without any notation
  = Strict
  -- | "!" can be used to reemphasize that a type is strict
  | ExplicitStrict
  -- | "~" means that a type is Lazy
  | Lazy
  deriving (Eq,Show,Read,Generic)

-- | An entity data row's type. If '_isEntityFieldTypeList' is 'True' than this type is a list.
data EntityFieldType = EntityFieldType {
  entityFieldTypeText   :: Text
, entityFieldStrictness :: Strictness
, entityFieldTypeList   :: Bool
, entityFieldTypeMaybe  :: Bool
} deriving (Eq,Read,Show,Generic)


-- | A unique idenfitier for an Entity.
data EntityUnique = EntityUnique {
  entityUniqueName            ::  Text
, entityUniqueEntityFieldName ::  [Text]
} deriving (Eq,Show,Read,Generic)

-- | 'deriving Eq', 'deriving Show', etc.
-- | There may be custom generic typeclasses
-- | so there is no restriction on what the type might be
-- | , other than it starts with a capital letter.
data EntityDerive = EntityDerive {
  entityDeriveTypes :: [Text]
} deriving (Eq,Show,Read,Generic)

-- | 'Primary name'
data EntityPrimary = EntityPrimary {
  entityPrimaryType :: [Text]
} deriving (Eq,Show,Read,Generic)

-- | 'Foreign Tree fkparent parent'
{-|
Module      : Database.Persist.Parser.Types
Description : Persistent model file parsing functions
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta
-}

data EntityForeign = EntityForeign {
  entityForeignTable :: Text
, entityForeignTypes :: [Text]
} deriving (Eq,Show,Read,Generic)

-- | Any white spaces that the user might want to maintain when generating Audit Models.
data WhiteSpace = WhiteSpace {
  whiteSpace :: Text
} deriving (Eq,Show,Read,Generic)

-- | Haskell style comments that start with "-- "
data Comment = Comment {
  comment :: Text
} deriving (Eq,Show,Read,Generic)

data MigrationOnlyAndSafeToRemoveOption = MigrationOnly
                                        | SafeToRemove
  deriving (Eq,Read,Show,Generic)

data EntityFieldLastItem = FieldDefault Text
                         | FieldSqlRow  Text
                         | FieldSqlType Text
                         | FieldMaxLen  Int
  deriving (Read,Show,Generic)

instance Eq EntityFieldLastItem where
  (FieldDefault  _) == (FieldDefault  _) = True
  (FieldSqlRow   _) == (FieldSqlRow   _) = True
  (FieldSqlType  _) == (FieldSqlType  _) = True
  (FieldMaxLen   _) == (FieldMaxLen   _) = True
  _ == _ = False
