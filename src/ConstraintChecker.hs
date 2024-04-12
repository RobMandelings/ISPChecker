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

checkConstraint (NandConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  return (not (r1 && r2))

checkConstraint (MinSPConstraint sp) = do
  (scope, isp) <- asks (\env -> (scope env, isp env))
  let courses = getCourses $ getCoursesWithYearFromCodes scope isp
  let totalSP = sum $ map (studyPoints) courses
  return (totalSP >= sp)

checkConstraint (MaxSPConstraint sp) = do
  (scope, isp) <- asks (\env -> (scope env, isp env))
  let courses = getCourses $ getCoursesWithYearFromCodes scope isp
  let totalSP = sum $ map (studyPoints) courses
  return (totalSP <= sp)

-- Const is a function that ignores its argument and returns a constant value.
-- We require const here because local expects a function that takes in an env and returns an adjusted environment.
-- But we have already created the newEnv via different variable, so we can just return that value.
checkConstraint (ScopedConstraint constraint newScope) = do
  env <- ask
  let newEnv = env { scope = newScope }
  local (const newEnv) (checkConstraint constraint)

-- Returns the corresponding
getCoursesWithYearFromCodes :: [CourseCode] -> ISP -> [CourseWithYear]
getCoursesWithYearFromCodes codes isp = mapMaybe (`Map.lookup` (courses isp)) codes

getCourses :: [CourseWithYear] -> [Course]
getCourses coursesWithYears = fmap fst coursesWithYears