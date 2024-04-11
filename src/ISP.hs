module ISP where

import Courses
import Data.Map.Strict as Map -- TODO: should I use Map.Strict or Map.Lazy?

type Year = Int
type CoursesByYear = Map.Map Year [Course]

type OptionName = String
type OptionValue = String
type Options = Map.Map OptionName OptionValue

data ISP = ISP
  { coursesByYear :: CoursesByYear
  , options :: Options -- Valid options are customised for each study program. For example, backgroundEducation or specialisation
  }