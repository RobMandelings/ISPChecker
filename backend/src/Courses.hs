module Courses where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

data Status = Passed | Planned Int deriving (Show)

type ISPCourse = (Course, Status)
type CourseCode = String

-- | Each course is given in a certain period.
data Period = FirstSem | SecondSem | AllYear deriving (Show, Eq, Ord, Generic)

-- | Course type that holds data for a single course
data Course = Course
  { name :: String
  , code :: CourseCode
  , description :: String
  , period :: Period
  , studyPoints :: Int
  } deriving (Show, Eq, Generic)

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON Course where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON Period where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions