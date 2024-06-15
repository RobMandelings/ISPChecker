module ISP where

import Courses
import qualified Data.Map.Strict as Map -- TODO: should I use Map.Strict or Map.Lazy?
import Data.Set (Set)
import qualified Data.Set as Set

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

getIncludedCourses :: ISP -> Set CourseCode
getIncludedCourses isp =
  let courseSel = courseSelection isp in
  let includedPlanned = foldr Set.union Set.empty $ planned courseSel in
    Set.union includedPlanned (passed courseSel)