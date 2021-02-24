{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database where

import Data.Int (Int64)
import Data.Profunctor.Product (p2)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as PGS
import Opaleye
  ( Field,
    SqlUuid,
    Table,
    rCount,
    runSelect,
    selectTable,
    tableField,
  )
import Opaleye.Manipulation (Insert (..))
import Opaleye.Select (Select)
import Opaleye.SqlTypes (SqlText, sqlStrictText)
import Opaleye.Table (table)

data IngredientName' a b = IngredientName
  { _ingredientId :: a,
    _ingredientName :: b
  }

type IngredientName = IngredientName' UUID Text

type IngredientNameField = IngredientName' (Field SqlUuid) (Field SqlText)

type IngredientNameTable = Table (Maybe (Field SqlUuid), Field SqlText) (Field SqlUuid, Field SqlText)

ingredientNameTable :: IngredientNameTable
ingredientNameTable =
  table
    "ingredient_name"
    ( p2 (tableField "id", tableField "name")
    )

createIngredientNameTable :: PGS.Connection -> IO Int64
createIngredientNameTable conn =
  PGS.execute_
    conn
    "CREATE TABLE IF NOT EXISTS \"ingredient_name\"\
    \( id UUID PRIMARY KEY DEFAULT uuid_generate_v4 (),\
    \  name TEXT NOT NULL\
    \)"

insertCheck :: Insert Int64
insertCheck =
  Insert
    { iTable = ingredientNameTable,
      iRows =
        [(Nothing, sqlStrictText "derp")],
      iReturning = rCount,
      iOnConflict = Nothing
    }

selectIngredientName :: Select (Field SqlUuid, Field SqlText)
selectIngredientName = selectTable ingredientNameTable

runSelectIngredientName :: PGS.Connection -> Select (Field SqlUuid, Field SqlText) -> IO [(UUID, Text)]
runSelectIngredientName = runSelect
