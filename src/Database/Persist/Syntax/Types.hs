{-|
Module      : Database.Persist.Syntax.Types
Description : Syntax tree for representing elements in Persistent model files
Copyright   : (c) James M.C. Haver II
License     : BSD3
Maintainer  : mchaver@gmail.com
Stability   : Beta

Attempt to represent all possible Persistent data in a syntax tree.

https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax
contains relatively up to date information about Persistent syntax.

https://github.com/yesodweb/persistent/tree/master/persistent-test/src
gives clues about newer syntax elements that have not been added to the wiki.

It is recommended that you import this qualified.
@import qualified Database.Persist.Syntax.Types as PST@
-}

{-# LANGUAGE DeriveGeneric #-}

module Database.Persist.Syntax.Types where

import           Data.Text (Text)
import           GHC.Generics

-- | The root of the Persistent syntax tree. A collection of data types with
-- which you can recontruct a Persist Model file or create an altered version.
type ModelsFile = [ModelsFilePiece]

-- | Top level pieces of a Persistent Model file.
data ModelsFilePiece = ModelsFileEntity     Entity     |
                       ModelsFileComment    Comment    |
                       ModelsFileWhiteSpace WhiteSpace
  deriving (Eq,Read,Show,Generic)

-- | A single Persist Model Entity.
data Entity = Entity {
  entityName       :: Text          -- ^ @Person@
, entityDeriveJson :: Bool          -- ^ @Person json@
, entitySqlTable   :: Maybe Text    -- ^ @Person sql=people@
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

-- | An EntityField corresponds to a column in SQL or a key-value pair in MongoDB.
-- The minimal definition of an EntityField in Persistent has a name and a type.
data EntityField = EntityField {
  entityFieldName            :: Text  -- ^ @name@, @address@, @age@
, entityFieldType            :: EntityFieldType -- ^ @Text@, @[Text]@, @Text Maybe@, @~Int@
, entityFieldIsMigrationOnly :: Bool  -- ^ @MigrationOnly@
, entityFieldIsSafeToRemove  :: Bool  -- ^ @SafeToRemove@
, entityFieldDefault         :: Maybe Text -- ^ @default=Nothing@, @default=now()@, @default=CURRENT_DATE@
, entityFieldSqlRow          :: Maybe Text -- ^ @sql=my_id_name@
, entityFieldSqlType         :: Maybe Text -- ^ @sqltype=varchar(255)@
, entityFieldMaxLen          :: Maybe Int  -- ^ @maxlen=3@
} deriving (Eq,Read,Show,Generic)


-- | Table rows can be strict or lazy
data Strictness
  -- | Persist Model types are strict without any notation
  = Strict
  -- | "!" can be used to reemphasize that a type is strict
  | ExplicitStrict
  -- | "~" means that a type is Lazy
  | Lazy
  deriving (Eq,Show,Read,Generic)

-- | An entity data row's type.
data EntityFieldType = EntityFieldType {
  entityFieldTypeText   :: Text -- ^ Text, Int, Double
, entityFieldStrictness :: Strictness -- ^ ~Text, !Text, Text
, entityFieldTypeList   :: Bool -- ^ [Text], [Int], [Double]
, entityFieldTypeMaybe  :: Bool -- ^ Text Maybe, Int Maybe, Double Maybe
} deriving (Eq,Read,Show,Generic)


-- | A unique idenfitier for an Entity: @UniqueUserName userIdent@,
-- @UniqueNameAndAge name age@.
data EntityUnique = EntityUnique {
  entityUniqueName            ::  Text -- ^@UniqueUserName@
, entityUniqueEntityFieldName ::  [Text] -- ^@userIdent@
} deriving (Eq,Show,Read,Generic)

-- | @deriving Eq@, @deriving Show@, etc.
-- There may be custom generic typeclasses
-- so there is no restriction on what the type might be
-- , other than it starts with a capital letter.
data EntityDerive = EntityDerive {
  entityDeriveTypes :: [Text] -- ^ deriving Eq, deriving Show
} deriving (Eq,Show,Read,Generic)

-- | 'Primary name'
data EntityPrimary = EntityPrimary {
  entityPrimaryType :: [Text]
} deriving (Eq,Show,Read,Generic)

-- | 'Foreign Tree fkparent parent'
data EntityForeign = EntityForeign {
  entityForeignTable :: Text
, entityForeignTypes :: [Text]
} deriving (Eq,Show,Read,Generic)

-- | White space found in the Persistent file or QuasiQuoter. Need to save the
-- white space in case you want to reproduce the original file or an altered version
-- of the file from the Persist Syntax Tree.
data WhiteSpace = WhiteSpace {
  whiteSpace :: Text
} deriving (Eq,Show,Read,Generic)

-- | Haskell style comments that start with @-- @ in Persistent.
data Comment = Comment {
  comment :: Text
} deriving (Eq,Show,Read,Generic)

-- | 'MigrationOnly' persistent-template \>= 1.2.0 marks a field that is ignored
-- by normal processing but retained for migration purposes. Useful for
-- implementing columns that other tools may need but Persistent does not.
-- 'SafeToRemove' is used to deprecate a field after 'MigrationOnly' has been
-- used. The field will be removed from the database if it is present. This is
-- a destructive change which is marked as safe by the user.
data MigrationOnlyAndSafeToRemoveOption = MigrationOnly
                                        | SafeToRemove
  deriving (Eq,Read,Show,Generic)

-- | These items may occur at the very end of a Field's line and in any order.
data EntityFieldLastItem = FieldDefault Text
                         | FieldSqlRow  Text
                         | FieldSqlType Text
                         | FieldMaxLen  Int
  deriving (Read,Show,Generic)

-- | Define equality to match based only on the type constructor.
instance Eq EntityFieldLastItem where
  (FieldDefault  _) == (FieldDefault  _) = True
  (FieldSqlRow   _) == (FieldSqlRow   _) = True
  (FieldSqlType  _) == (FieldSqlType  _) = True
  (FieldMaxLen   _) == (FieldMaxLen   _) = True
  _ == _ = False

{-
eqConstructor :: EntityFieldLastItem -> EntityFieldLastItem -> Bool
eqConstructor (FieldDefault  _) (FieldDefault  _) = True
eqConstructor (FieldSqlRow   _) (FieldSqlRow   _) = True
eqConstructor (FieldSqlType  _) (FieldSqlType  _) = True
eqConstructor (FieldMaxLen   _) (FieldMaxLen   _) = True
eqConstructor _ _ = False
-}
