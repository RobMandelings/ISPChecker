-- Required to define the type of the instance inline instead of having to wrap it in a different type
{-# LANGUAGE FlexibleInstances #-}

module ConstraintChecker where

-- TODO why did I use a strict map here?
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap -- Forces evaluation of values when they are inserted into the map. As opposed to normal map.
import Data.Maybe (mapMaybe)
import Control.Monad.Reader
import qualified Data.Set as Set

import qualified Courses
import StudyProgram
import Constraints
import ISP

-- TODO
-- the lhs defines the type constraint for this typeclass.
-- Any m that is an instance of the CourseStore type class, should also be an instance of the Monad typeclass

--class Monad m => CourseStore m where
--  getCourse :: Courses.CourseCode -> m (Maybe Courses.Course)

data CourseStore = CourseStore { getCourse :: Courses.CourseCode -> Maybe Courses.Course }

-- | Maps the course code to the corresponding course
type CourseMap = Map.Map Courses.CourseCode Courses.Course

createMapCourseStore :: CourseMap -> CourseStore
createMapCourseStore courseMap =
  CourseStore { getCourse = (\courseCode -> Map.lookup courseCode courseMap) }

createDBCourseStore :: String -> CourseStore
createDBCourseStore dbName =
  CourseStore { getCourse = (\courseCode -> Nothing) }

-- Instances don't work because they will always return a monad and you still need to know how to execute it and pass the proper parameters.

--instance CourseStore (Reader CourseMap) where
--  getCourse courseCode = do
--    courseMap <- ask
--    return $ Map.lookup courseCode courseMap

--createGetCourseFromMap

-- runReader (getCourse courseCode) courseMap (gives back the actual result)

--instance CourseStore IO where
--  getCourse courseCode = do
--    return Nothing -- TODO implement database implementation



data CourseStoreEnv = CourseStoreEnv
  { runStore :: Courses.CourseCode -> (Maybe Courses.Course)}


--createEnv :: CourseStore m => (Courses.CourseCode -> m (Maybe Courses.Course)) -> (m a -> a) -> CourseStoreEnv
--createEnv getCourse runMonad = CourseStoreEnv $ \courseCode -> runMonad (getCourse courseCode)

--instance CourseStore (ReaderT CourseMap IO) where
--  getCourse courseCode = do
--    courseMap <- ask
--    return $ Map.lookup courseCode courseMap


data Env = Env
  {
    isp :: ISP
  , courseStore :: CourseMap
  }

type ConstraintChecker = ReaderT Env Maybe Bool

extractCourse :: ConstraintChecker
extractCourse = do
  error "Hi"
--
---- check whether module is active
---- evaluate subModules
--
---- create and constraint merging all constraints together
---- get the scope (course codes)
---- wrap the andConstraint in a scopedConstraint (todo for organisational purposes maybe its better to iterate over the constraints?)
---- evaluate this constraint
--
--isActive :: Module -> ISP -> Bool
--isActive mod isp = (activator mod) (options isp)
--
--checkModule :: Module -> ConstraintChecker
--checkModule mod = do
--  isp <- ask
--  if isActive mod isp
--  then do
--    let scope = getScope mod isp
--    -- Not only applies checkConstraint to each constraint in the list, but also sequences the results in a single monadic action that
--    -- Produces all results. If at least one result returned Nothing, the binding fails and checkModule will return Nothing as well.
--    subModuleResults <- mapM checkModule (subModules mod)
--    results <- mapM (\c -> checkConstraint (ScopedConstraint c scope)) (constraints mod)
--    return $ all id results -- (all :: (a -> Bool) -> [a] -> Bool. First argument is the predicate (in this case id function, because results are already booleans)
--  else return True
--
--checkConstraint :: Constraint -> ConstraintChecker
--checkConstraint (IncludedConstraint code) = do
--  isp <- ask
--  return (StrictMap.member code (courseSelection isp))
--
--checkConstraint (NandConstraint c1 c2) = do
--  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
--  r2 <- checkConstraint c2
--  return (not (r1 && r2))
--
--checkConstraint (MinSPConstraint sp) = do
--  isp <- ask
--  let courses = getCourses $ StrictMap.elems $ courseSelection isp
--  let totalSP = sum $ map (studyPoints) courses
--  return (totalSP >= sp)
--
--checkConstraint (MaxSPConstraint sp) = do
--  isp <- ask
--  let courses = getCourses $ StrictMap.elems $ courseSelection isp
--  let totalSP = sum $ map (studyPoints) courses
--  return (totalSP <= sp)
--
---- Const is a function that ignores its argument and returns a constant value.
---- We require const here because local expects a function that takes in an env and returns an adjusted environment.
---- But we have already created the newEnv via different variable, so we can just return that value.
--checkConstraint (ScopedConstraint constraint newScope) = do
--  isp <- ask
--  let newISP = filterISP isp newScope
--  local (const newISP) (checkConstraint constraint)
----
--checkConstraint (SameYearConstraint code1 code2) = do
--  isp <- ask
--  let selectionMap = courseSelection isp
--  -- The below implementation does not handle the situation where one of two lookups return nothing, the constraint check returns nothing (this is an error, something went wrong here).
--  return $ case (StrictMap.lookup code1 selectionMap, StrictMap.lookup code2 selectionMap) of
--    (Just (_, Planned year1), Just (_, Planned year2)) -> year1 == year2
--    (Just (_, Passed), Just (_, Passed)) -> True
--    _ -> False -- TODO THIS SHOULD BE NOTHING INSTEAD!!! If nothing is returned, then something went wrong here
--
----remainingSPConstraint (RemainingSPConstraint sp) = do
----  isp <- ask
--
--getScope :: Module -> ISP -> [Courses.CourseCode]
--getScope mod isp =
--  if isActive mod isp
--  then courses mod ++ concatMap (\subMod -> getScope subMod isp) (subModules mod)
--  else []
--
--filterISP :: ISP -> Scope -> ISP
--filterISP isp scope =
--  let scopeSet = Set.fromList scope -- More efficient lookups. TODO: maybe scope should always be a set if possible?
--      filteredSelection = StrictMap.filterWithKey (\key _ -> Set.member key scopeSet) (courseSelection isp) -- Underscore ignores the value associated with that key. Is a wildcard.
--  in isp { courseSelection = filteredSelection }
--
--getCourses :: [Courses.ISPCourse] -> [Courses.Course]
--getCourses ispCourses = fmap fst ispCourses
--
--runCheckModule :: Module -> ISP -> Maybe Bool
--runCheckModule mod isp = runReaderT (checkModule mod) isp