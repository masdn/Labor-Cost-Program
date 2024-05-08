
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import qualified DBCredentials as CREDS

--data aConnection = Connect Connection
{-}
data BasicProduct = BasicProduct {sku :: Text
                                  , size :: Int
                                  , price :: Int
                                  , description :: Maybe Text}
  deriving (Show, Generic)
  deriving anyclass (ToRow, FromRow)

poView :: Connection -> (Int, Int, Int, Int, Int) -> Widget () 
poView db (sb, x, h, w, c) = renderTable $ table
    [ [ str "Item Qty"  , str "Length" ,   str "Description"  ]
    , [ str (show 2)    , str (show w)     , str ((getDesc db w sb)) ]
    , [ str (show 2)    , str (show h)     , str "COL3" ]
    , [ str "---"          , str "----"    , str "---"  ]
    ]


getDesc :: Connection -> Int -> Int -> BasicProduct
getDesc db size barType = 
    case barType of
      1 -> queryData db size
      2 -> queryData db size
      3 -> queryData db size

queryData :: Connection -> Int -> IO String
queryData connection s =  do
  let sqlQuery = "SELECT id,description FROM inventory WHERE sku = 'LD' AND size = ?"
  xs <- query connection sqlQuery (Only s)
  let formattedResults = unlines $ map (\(id, description) ->
            "ID: " ++ show id ++ ", SKU: " ++ unpack description) xs
  return formattedResults
    
-}
run :: (Int, Int, Int, Int, Int)  -> IO ()
run t@(s ,xx, h, w, c) = do
    dbConnection <- getConnection
    --queryData dbConnection h
    displayPoView dbConnection t 
    return ()

data InventoryDesc = InventoryDesc
  { sku :: Text
  , description :: Text
  } deriving (Eq)

instance Show InventoryDesc where
  show (InventoryDesc _ description ) = unpack description

-- Implement the FromRow instance to map the query result to the Inventory data type
instance FromRow InventoryDesc where
  fromRow = InventoryDesc <$> field <*> field

type Real = Double

data InventoryPrice = InventoryPrice
  { sku1 :: Text
  , price :: Int
  } deriving (Eq)

instance Show InventoryPrice where
  show (InventoryPrice _ price ) = show price


-- Implement the FromRow instance to map the query result to the Inventory data type
instance FromRow InventoryPrice where
  fromRow = InventoryPrice <$> field <*> field

queryData :: Connection -> Int -> IO [InventoryDesc]
queryData connection s =  do
  let sqlQuery = "SELECT sku,description FROM inventory WHERE sku LIKE '%LD%' AND size = (?)"
  query connection sqlQuery (Only s) :: IO [InventoryDesc]
 -- forM_ xs $ \(Inventory _ description) -> putStrLn (unpack description)
{-
-- Function to query data and return a string description
queryData1 :: Connection -> Int -> IO String
queryData1 connection s = do
    let sqlQuery = "SELECT description FROM inventory WHERE sku = 'LD' AND size = ?"
    xs <- query connection sqlQuery (Only s)
    let formattedResults = case xs of
            [(Just desc)] -> unpack desc
            _ -> "Not Found"
    return formattedResults
-}
-- Function to return a description based on bar type
getDesc :: Connection -> Int -> Int -> IO [InventoryDesc]
getDesc db size barType = case barType of
    1 -> queryData db size
    2 -> queryData db size
    3 -> queryData db size
    _ -> return []

getPrice :: Connection -> Int -> Int -> IO [InventoryPrice]
getPrice db size barType = case barType of
    1 -> queryPrice db size
    2 -> queryPrice db size
    3 -> queryPrice db size
    _ -> return []

queryPrice :: Connection -> Int -> IO [InventoryPrice]
queryPrice connection s =  do
  let sqlQuery = "SELECT sku,price FROM inventory WHERE sku LIKE '%LD%' AND size = (?)"
  query connection sqlQuery (Only s) :: IO [InventoryPrice]

-- Function to return a table widget
poView :: Connection -> (Int, Int, Int, Int, Int) -> IO (Widget ())
poView db (sb, x, h, w, c) = do
    desc1 <- getDesc db w sb
    desc2 <- getDesc db h sb
    price1 <- (getPrice db w sb ) 
    price2 <- (getPrice db h sb ) 
    --length <- getLgenth db w sb
    let tableData = table
            [ [ str "Item Qty"  , str "Length" ,   str "Description" , str "Price"]
            , [ str (show 2)    , str (show w)     , str (show desc1), str (show price1)  ]
            , [ str (show 2)    , str (show h)     , str (show desc2), str (show price2) ]
            , [ str "---"       , str "----"       , str "---" , str "" ] 
            , [ str "---"       , str "---"       , str "Labor Cost:", str (show $ calcLabor h w )]
            ]
    return $ renderTable tableData



calcLabor :: Int -> Int -> Float
calcLabor l w = fromIntegral (l + w) * 0.92

-- Function to display the UI with the table widget
displayPoView :: Connection -> (Int, Int, Int, Int, Int) -> IO ()
displayPoView db canvas = do
    widget <- poView db canvas
    void $ simpleMain widget

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


queryLD :: Connection -> IO ()
queryLD connection = do
  xs <- query_ connection "select id,description from inventory where description like '%Light%'"
  --print (xs :: [Only Int])
  forM_ xs $ \(id,description) ->
    putStrLn $ unpack description ++ " has ID: " ++ show (id :: Int)
  --putStrLn $ "Query 1: " <> show print 


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