module ConstraintChecker where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Control.Monad.Reader
import qualified Data.Set as Set

import Courses
import Constraints
import ISP

type ConstraintChecker = ReaderT ISP Maybe

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
  return False
--  let courses = getCourses $ Map.elems $ courses isp
--  let totalSP = sum $ map (studyPoints) courses
--  return (totalSP <= sp)

-- Const is a function that ignores its argument and returns a constant value.
-- We require const here because local expects a function that takes in an env and returns an adjusted environment.
-- But we have already created the newEnv via different variable, so we can just return that value.
checkConstraint (ScopedConstraint constraint newScope) = do
  isp <- ask
  let newISP = filterISP isp newScope
  local (const newISP) (checkConstraint constraint)
--
--checkConstraint (SameYearConstraint code1 code2) = do
--  (scope, isp) <- asks (\env -> (scope env, isp env))
--  let scopedCourseMap = courses $ filterISP isp scope
--



filterISP :: ISP -> Scope -> ISP
filterISP isp scope =
  let scopeSet = Set.fromList scope -- More efficient lookups. TODO: maybe scope should always be a set if possible?
      filteredSelection = Map.filterWithKey (\key _ -> Set.member key scopeSet) (courseSelection isp) -- Underscore ignores the value associated with that key. Is a wildcard.
  in isp { courseSelection = filteredSelection }

getCourses :: [CourseWithYear] -> [Course]
getCourses coursesWithYears = fmap fst coursesWithYears