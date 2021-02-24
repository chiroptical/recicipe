{-# LANGUAGE DataKinds #-}

module Main where

-- import Network.Wai.Handler.Warp (run)
-- import Routes (app)

import Database
import Database.PostgreSQL.Simple (connectPostgreSQL)

main :: IO ()
main = do
  -- run 8081 app
  conn <- connectPostgreSQL "dbname=recicipe user=chiroptical"
  -- createIngredientNameTable conn
  -- rowsInserted <- runInsert_ conn insertCheck
  rows <- runSelectIngredientName conn selectIngredientName
  print rows
