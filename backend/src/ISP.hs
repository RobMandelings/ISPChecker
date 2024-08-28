module ISP where

import Courses
import qualified Data.Map.Strict as Map -- TODO: should I use Map.Strict or Map.Lazy?
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

-- | The course selection of a student. This includes the courses that have been passed and the courses that are planned.
data CourseSelection = CourseSelection
  { passed :: Set CourseCode
  , planned :: [Set CourseCode]
  } deriving (Show, Generic)

-- | The name of the option
type OptionName = String

-- | The value of the option
type OptionValue = String

-- | The options of the ISP. This is a map from the option name to the option value. (e.g. speciaisation: AI, background: informatica)
type ISPOptions = Map.Map OptionName OptionValue

-- | The ISP (Individual Study Program) of a student. This includes the course selection, the study program and the options (the options are customised for each study program).
data ISP = ISP
  { courseSelection :: CourseSelection
  , studyProgram :: String
  , options :: ISPOptions -- Valid options are customised for each study program. For example, backgroundEducation or specialisation
  } deriving (Show, Generic)

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON CourseSelection where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON ISP where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | Retrieves an option from the ISP using the given name. If the option is not found, an error is thrown.
-- | This eliminates the need for manual checking of the errors.
getOption :: ISP -> OptionName -> OptionValue
getOption isp name =
  case (Map.lookup name isp.options) of
    Just val ->
      val
    Nothing ->
      error $ "Couldn't find the option " ++ show name ++ " in the ISP"

-- | Retrieves the courses that are included in the ISP. This includes the courses that are planned and the courses that have been passed (so the entire set of courses that are included in the ISP).
getIncludedCourses :: ISP -> Set CourseCode
getIncludedCourses isp =
  let courseSel = isp.courseSelection in
  let includedPlanned = foldr Set.union Set.empty $ courseSel.planned in
    Set.union includedPlanned $ courseSel.passed

-- Either adds a new set of course codes to the list (indicating a new year),
-- or takes the union of the current set and the last set in the list (appends a semester of coursecodes to this year)

-- | Folds over the course selection to get the planned courses per year.
foldStep :: (Int, Set CourseCode) -> [Set CourseCode] -> [Set CourseCode]
foldStep (i, courseCode) acc =
  if i `mod` 2 == 0 then
    acc ++ [courseCode] -- Inefficient for longer lists, if performance is concern use a difference method
  else
    let courseCodesInYear = Set.union (last acc) (courseCode) in
      (init acc) ++ [courseCodesInYear]

-- | Retrieves the planned courses per year from the course selection.
getPlannedPerYear :: CourseSelection -> [Set CourseCode]
getPlannedPerYear courseSel =
  let plannedSel = courseSel.planned in
  let plannedPerYear = foldr foldStep [] (zip [0..] []) in
      plannedPerYear