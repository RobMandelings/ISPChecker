module ConstraintChecker where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Control.Monad.Reader

import Courses
import Constraints
import ISP

data Env = Env
  { isp :: ISP
  , scope :: Scope
  }

type ConstraintChecker = ReaderT Env Maybe

checkConstraint :: Constraint -> ConstraintChecker Bool
checkConstraint (MinSPConstraint sp) = do
  (scope, isp) <- asks (\env -> (scope env, isp env))
  let courses = getCourses $ getCoursesWithYearFromCodes scope isp
  let totalSP = sum $ map (studyPoints) courses
  return (totalSP >= sp)

-- Returns the corresponding
getCoursesWithYearFromCodes :: [CourseCode] -> ISP -> [CourseWithYear]
getCoursesWithYearFromCodes codes isp = mapMaybe (`Map.lookup` (courses isp)) codes

getCourses :: [CourseWithYear] -> [Course]
getCourses coursesWithYears = fmap fst coursesWithYears