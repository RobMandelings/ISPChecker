module Courses where

type Year = Int
type CourseWithYear = (Course, Year)

data Semester = First | Second deriving (Show, Eq, Ord)
data Course = Course
  { name :: String
  , description :: String
  , semester :: Semester
  , studyPoints :: Int
  } deriving (Show, Eq)

bedrijfskunde :: Course
bedrijfskunde = Course
  { name = "Bedrijfskunde en Entrepreneurship"
  , description = "Voor bedrijfskunde te leren"
  , semester = First
  , studyPoints = 6
  }