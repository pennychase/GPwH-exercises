module DatabaseCommands where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import SchemaTypes

dbName :: String 
dbName = "tools.db"

-- Abstract connecting to the database
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

-- Create User
addUser :: String -> IO ()
addUser userName = withConn dbName $
    \conn -> do
        execute conn "INSERT INTO users (username) VALUES (?)"
            (Only userName)
        print "user added"

-- Create Tool
-- Use defaults for lastReturned (today) and timesBorrowed (0)
addTool :: String -> String -> IO ()
addTool name description = withConn dbName $
    \conn -> do
        today <- utctDay <$> getCurrentTime
        let timesBorrowed = 0 :: Int
        execute conn "INSERT INTO tools (name, description, lastReturned, timesBorrowed) VALUES (?,?,?,?)"
            (name, description, today, timesBorrowed)

-- Create Checkout
checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn dbName $
    \conn -> do
        execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)"
            (userId, toolId)

-- Print Users
printUsers :: IO ()
printUsers = withConn dbName $
    \conn -> do
        resp <- query_ conn "SELECT * FROM users;" :: IO [User]
        mapM_ print resp

-- Update Tool

-- Find tool by ID
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn "SELECT * FROM tools WHERE id = (?)"
            (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp

-- firstOrNothing looks at the results returned by the query and returns
-- Nothing if not found or the head of the list, which should be a singleton since IDs are unique
firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing 
firstOrNothing (x:_) = Just x

-- update tool by incrementing timesBorrowed and using today's date for return date
updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    { lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
    }

-- the actual database update, but only if the tool had been found
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) =
    withConn dbName $
        \conn -> do
            let q = mconcat [ "UPDATE TOOLS SET "
                            , "lastReturned = ?, "
                            , "timesBorrowed = ? "
                            , "WHERE ID = ?;"
                            ]
            execute conn q ( lastReturned tool
                            , timesBorrowed tool
                            , toolId tool)
            print "tool updated"

-- putting the tool update together
updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn dbName $
    \conn -> do
        tool <- selectTool conn toolId
        currentDay <- utctDay <$> getCurrentTime 
        let updatedTool = updateTool <$> tool <*> pure currentDay
        updateOrWarn updatedTool

-- Delete checkedout data when a tool is checked in

checkin :: Int -> IO ()
checkin toolId = withConn dbName $
    \conn -> do
        execute conn "DELETE FROM checkedout WHERE tool_id = (?);"
            (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

-- Tool Queries
-- Want to get all tools, checked out tools, and available tools

-- Generic Query
printToolQuery :: Query -> IO ()
printToolQuery q = withConn dbName $
    \conn -> do
        resp <- query_ conn q :: IO [Tool]
        mapM_ print resp

-- all tools
printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

-- available tools
printAvailable :: IO ()
printAvailable = printToolQuery $ 
                 mconcat [ "select * from tools "
                         , "where id not in "
                         , "(select tool_id from checkedout);"
                         ]

-- checkout tools
printCheckedOut :: IO ()
printCheckedOut = printToolQuery $
                  mconcat [ "select * from tools "
                          , "where id in "
                          , "(select tool_id from checkedout);"
                          ]


