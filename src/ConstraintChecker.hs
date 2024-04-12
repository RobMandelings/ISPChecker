module ConstraintChecker where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Control.Monad.Reader
import qualified Data.Set as Set

import Courses
import StudyProgram
import Constraints
import ISP

type ConstraintChecker = ReaderT ISP Maybe

-- check whether module is active
-- evaluate subModules

-- create and constraint merging all constraints together
-- get the scope (course codes)
-- wrap the andConstraint in a scopedConstraint (todo for organisational purposes maybe its better to iterate over the constraints?)
-- evaluate this constraint
checkModule :: Module -> ConstraintChecker Bool
checkModule mod = do
  return False

checkConstraint :: Constraint -> ConstraintChecker Bool
checkConstraint (IncludedConstraint code) = do
  isp <- ask
  return (Map.member code (courseSelection isp))

checkConstraint (NandConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  return (not (r1 && r2))

checkConstraint (MinSPConstraint sp) = do
  isp <- ask
  let courses = getCourses $ Map.elems $ courseSelection isp
  let totalSP = sum $ map (studyPoints) courses
  return (totalSP >= sp)

checkConstraint (MaxSPConstraint sp) = do
  isp <- ask
  let courses = getCourses $ Map.elems $ courseSelection isp
  let totalSP = sum $ map (studyPoints) courses
  return (totalSP <= sp)

-- Const is a function that ignores its argument and returns a constant value.
-- We require const here because local expects a function that takes in an env and returns an adjusted environment.
-- But we have already created the newEnv via different variable, so we can just return that value.
checkConstraint (ScopedConstraint constraint newScope) = do
  isp <- ask
  let newISP = filterISP isp newScope
  local (const newISP) (checkConstraint constraint)
--
checkConstraint (SameYearConstraint code1 code2) = do
  isp <- ask
  let selectionMap = courseSelection isp
  -- The below implementation does not handle the situation where one of two lookups return nothing, the constraint check returns nothing (this is an error, something went wrong here).
  return $ case (Map.lookup code1 selectionMap, Map.lookup code2 selectionMap) of
    (Just (_, Planned year1), Just (_, Planned year2)) -> year1 == year2
    (Just (_, Passed), Just (_, Passed)) -> True
    _ -> False -- TODO THIS SHOULD BE NOTHING INSTEAD!!! If nothing is returned, then something went wrong here

--remainingSPConstraint (RemainingSPConstraint sp) = do
--  isp <- ask

getScope :: Module -> ISP -> [CourseCode]
getScope mod isp = undefined

filterISP :: ISP -> Scope -> ISP
filterISP isp scope =
  let scopeSet = Set.fromList scope -- More efficient lookups. TODO: maybe scope should always be a set if possible?
      filteredSelection = Map.filterWithKey (\key _ -> Set.member key scopeSet) (courseSelection isp) -- Underscore ignores the value associated with that key. Is a wildcard.
  in isp { courseSelection = filteredSelection }

getCourses :: [ISPCourse] -> [Course]
getCourses ispCourses = fmap fst ispCourses