module SchemaTypes where

import Data.Time.Calendar (Day)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data Tool = Tool
    { toolId :: Int
    , name :: String 
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n times borrowed: "
                        , show $ timesBorrowed tool
                        , "\n"]

instance FromRow Tool where
    fromRow = Tool  <$> field 
                    <*> field 
                    <*> field 
                    <*> field 
                    <*> field
                        
data User = User
    { userId :: Int
    , userName :: String
    }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user
                        ]

instance FromRow User where
    fromRow = User <$> field 
                   <*> field
