module ISP where

import Courses
import qualified Data.Map.Strict as Map -- TODO: should I use Map.Strict or Map.Lazy?
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

-- TODO What about more flexible ISP's where you can enter the year arbitrarily (for later)

data CourseSelection = CourseSelection
  { passed :: Set CourseCode
  , planned :: [Set CourseCode]
  } deriving (Show, Generic)

type OptionName = String
type OptionValue = String
type ISPOptions = Map.Map OptionName OptionValue

data ISP = ISP
  { courseSelection :: CourseSelection
  , studyProgram :: String
  , options :: ISPOptions -- Valid options are customised for each study program. For example, backgroundEducation or specialisation
  } deriving (Show, Generic)

instance Aeson.ToJSON CourseSelection where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON ISP where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions


getIncludedCourses :: ISP -> Set CourseCode
getIncludedCourses isp =
  let courseSel = isp.courseSelection in
  let includedPlanned = foldr Set.union Set.empty $ courseSel.planned in
    Set.union includedPlanned $ courseSel.passed

-- Either adds a new set of course codes to the list (indicating a new year),
-- or takes the union of the current set and the last set in the list (appends a semester of coursecodes to this year)

foldStep :: (Int, Set CourseCode) -> [Set CourseCode] -> [Set CourseCode]
foldStep (i, courseCode) acc =
  if i `mod` 2 == 0 then
    acc ++ [courseCode] -- Inefficient for longer lists, if performance is concern use a difference method
  else
    let courseCodesInYear = Set.union (last acc) (courseCode) in
      (init acc) ++ [courseCodesInYear]

getPlannedPerYear :: CourseSelection -> [Set CourseCode]
getPlannedPerYear courseSel =
  let plannedSel = courseSel.planned in
  let plannedPerYear = foldr foldStep [] (zip [0..] []) in
      plannedPerYear