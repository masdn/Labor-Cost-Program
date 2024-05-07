module DBController where

import Control.Exception (Exception (displayException), handle, throwIO)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics (Generic)
import qualified DBCredentials as CREDS

runThis :: IO ()
runThis = do
  putStrLn "\nRunning postgresql-simple"
  connection <- getConnection
  {- cleanUp connection
  insertStuff connection
  queryData connection
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