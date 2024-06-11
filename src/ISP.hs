module ISP where

import Courses
import Data.Map.Strict as Map -- TODO: should I use Map.Strict or Map.Lazy?

type ISPCourseSelection = Map.Map CourseCode ISPCourse

type OptionName = String
type OptionValue = String
type ISPOptions = Map.Map OptionName OptionValue

data ISP = ISP
  { courseSelection :: ISPCourseSelection
  , options :: ISPOptions -- Valid options are customised for each study program. For example, backgroundEducation or specialisation
  } deriving (Show)