module Courses where

data Status = Passed | Planned Int

type ISPCourse = (Course, Status)
type CourseCode = String

--data Status = Finished | Planned Int

data Period = FirstSem | SecondSem | AllYear deriving (Show, Eq, Ord)
data Course = Course
  { name :: String
  , code :: CourseCode
  , description :: String
  , period :: Period
  , studyPoints :: Int
  } deriving (Show, Eq)

bedrijfskunde :: Course
bedrijfskunde = Course
  { name = "Bedrijfskunde en Entrepreneurship"
  , code = "H01F2A"
  , description = ""
  , period = FirstSem
  , studyPoints = 6
  }

industrial_internet_infrastructure :: Course
industrial_internet_infrastructure = Course
  { name = "Industrial Internet Infrastructure"
  , code = "H04I0A"
  , description = ""
  , period = SecondSem
  , studyPoints = 5
  }

capita_ds :: Course
capita_ds = Course
  { name = "Capita Selecta Computer Science: Distributed Systems"
  , code = "H04G7A"
  , description = ""
  , period = AllYear
  , studyPoints = 4
  }