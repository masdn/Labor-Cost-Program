
{-# LANGUAGE OverloadedStrings #-}
module DBController where

import Brick.Main (simpleMain)
import Brick.Types (Widget)
import Brick.Widgets.Table (table, renderTable)
import Brick.Widgets.Core (str)
import Control.Exception (Exception (displayException), handle, throwIO)
import Control.Monad (void, forM_)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import qualified DBCredentials as CREDS



poView :: (Int, Int, Int, Int, Int) -> Widget () 
poView (sb, x, h, w, c) = renderTable $ table
    [ [ str "Item Qty"      , str "Description" , str ""  ]
    , [ str (show 2)    , str (show w)       , str "COL3" ]
    , [ str (show 2)    , str (show h)       , str "COL3" ]
    , [ str ""          , str ""             , str ""     ]
    ]

--getSku :: Int -> Int -> String


run :: (Int, Int, Int, Int, Int)  -> IO ()
run t@(s ,xx, h, w, c) = do
    connection <- getConnection
    queryData connection h
    simpleMain (poView t)
    return ()

runThis :: (Int, Int, Int, Int, Int) -> IO ()
runThis (s ,xx, h, w, c) = do
  putStrLn "\nRunning postgresql-simple"
  connection <- getConnection
  queryData connection h
  --queryLD connection
  {- cleanUp connection
  insertStuff connection
  
  insertWithTransaction connection
  queryWithJoins connection
  errors connection
  close connection -}
  putStrLn "Done"

getConnection :: IO Connection
getConnection =
  connect
    $ defaultConnectInfo
      { connectHost = CREDS.host
      , connectDatabase = CREDS.database
      , connectUser = CREDS.user
      , connectPassword = CREDS.password
      }

type FromInventory = (Int64, Text, Maybe Text) 

queryData :: Connection -> Int -> IO ()
queryData connection s =  do
  let sqlQuery = "SELECT id, sku FROM inventory WHERE sku = 'LD' AND size = ?"
  xs <- query connection sqlQuery (Only s)
  --print (xs :: [Only Int])
  forM_ xs $ \(id,description) ->
    putStrLn $ unpack description ++ " is " ++ show (id :: Int)
  --putStrLn $ "Query 1: " <> show print

queryLD :: Connection -> IO ()
queryLD connection = do
  xs <- query_ connection "select id,description from inventory where description like '%Light%'"
  --print (xs :: [Only Int])
  forM_ xs $ \(id,description) ->
    putStrLn $ unpack description ++ " has ID: " ++ show (id :: Int)
  --putStrLn $ "Query 1: " <> show print 

queryData1 :: Connection -> IO ()
queryData1 connection = do
    [Only i] <- query_ connection "select 2 + 2"
    return i 

printQueryRow :: (Int64, Text, Maybe Text) -> IO ()
printQueryRow (id, sku, description) =
    putStrLn $ "ID: " <> show id <> ", SKU: " <> unpack sku <> ", Description: " <> maybe "N/A" unpack description

{-
calculateLaborC :: (Int, Int, Int, Int, Int)
calculateLaborC bars X hght wdth canvas = do
    case bars of
      1 -> queryLD -}
{-
type InventoryRecord = (Int64, Text, Maybe Text)

data Query where
    Query :: [(Int64, Text, Maybe Text)]

printQueryRow :: InventoryRecord -> IO ()
printQueryRow (id, sku, description) =
    putStrLn $ "ID: " <> show id <> ", SKU: " <> unpack sku <> ", Description: " <> maybe "N/A" unpack description

queryData :: Connection -> IO ()
queryData connection = do
  query1 :: [(Int64, Text, Maybe Text)] <- query_ connection "select id, sku, description from inventory"
  putStrLn $ "Query 1: " <> show query1 -}