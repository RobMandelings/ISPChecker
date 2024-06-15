module ISP where

import Courses
import Data.Map.Strict as Map -- TODO: should I use Map.Strict or Map.Lazy?
import Data.Set (Set)

data CourseSelection = CourseSelection
  { passed :: Set CourseCode
  , planned :: [Set CourseCode]
  } deriving (Show)

type OptionName = String
type OptionValue = String
type ISPOptions = Map.Map OptionName OptionValue

data ISP = ISP
  { courseSelection :: CourseSelection
  , studyProgram :: String
  , options :: ISPOptions -- Valid options are customised for each study program. For example, backgroundEducation or specialisation
  } deriving (Show)