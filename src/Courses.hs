module Courses where

data Status = Passed | Planned Int

type ISPCourse = (Course, Status)
type CourseCode = String

--data Status = Finished | Planned Int

data Semester = First | Second deriving (Show, Eq, Ord)
data Course = Course
  { name :: String
  , code :: CourseCode
  , description :: String
  , semester :: Semester
  , studyPoints :: Int
  } deriving (Show, Eq)

bedrijfskunde :: Course
bedrijfskunde = Course
  { name = "Bedrijfskunde en Entrepreneurship"
  , code = "H01F2A"
  , description = "Voor bedrijfskunde te leren"
  , semester = First
  , studyPoints = 6
  }