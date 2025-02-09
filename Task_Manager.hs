{-# LANGUAGE DeriveGeneric #-}
module TaskManager where

import System.IO
import Data.Time
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import System.Directory

data Priority = Low | Medium | High
  deriving (Show, Read, Generic, Eq)

data Task = Task 
  { taskId :: Int
  , title :: String
  , description :: String
  , priority :: Priority
  , dueDate :: String
  , completed :: Bool
  } deriving (Show, Read, Generic)

instance ToJSON Priority
instance FromJSON Priority
instance ToJSON Task
instance FromJSON Task
