{-# LANGUAGE DeriveGeneric #-}
-- Required to define the type of the instance inline instead of having to wrap it in a different type
module ConstraintChecker where

-- TODO why did I use a strict map here?
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap -- Forces evaluation of values when they are inserted into the map. As opposed to normal map.
import Data.Maybe (mapMaybe)
import Control.Monad.Reader
import qualified Data.Set as Set
import Data.Text (pack, Text)

import qualified Courses
import StudyProgram
import qualified Constraints
import ISP (ISP)
import qualified ISP as ISP
import Debug.Trace
import Text.Show.Pretty (ppShow)
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

class ToBool a where
  toBool :: a -> Bool

data ConstraintResult =
  ConstraintSuccess |
  ConstraintFail {
    errorMsg :: Text,
    subResults :: [ConstraintResult]
  } deriving (Show, Generic)

data ModuleResult =
  ModuleSuccess |
  ModuleFail {
    constraintResults :: [ConstraintResult],
    subModuleResults :: [ModuleResult]
  } deriving (Show, Generic)

instance Aeson.ToJSON ConstraintResult where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON ModuleResult where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance ToBool ModuleResult where
  toBool ModuleSuccess = True
  toBool (ModuleFail _ _) = False -- TODO cleaner way to handle this?

instance ToBool ConstraintResult where
  toBool ConstraintSuccess = True
  toBool (ConstraintFail _ _) = False

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

data Env = Env
  {
    isp :: ISP
  , courseStore :: CourseStore
  }

type ModuleChecker = ReaderT Env Maybe ModuleResult
type ConstraintChecker = ReaderT Env Maybe ConstraintResult
--
---- check whether module is active
---- evaluate subModules
--
---- create and constraint merging all constraints together
---- get the scope (course codes)
---- wrap the andConstraint in a scopedConstraint (todo for organisational purposes maybe its better to iterate over the constraints?)
---- evaluate this constraint
--

-- No need to use a monad here, just use getCourses
-- No monad because its not easy to change the monad type with a different environment. You use need to use runReader to extract the values, but then that would be the same as just having an argument.
-- If the monadic context for getCourses is used in recursive calls, then you might want to reconsider using a monad. Otherwise I wouldn't see the point.
getCourses :: Env -> [Courses.Course]
getCourses env =
    let courseIds = ISP.getIncludedCourses $ env.isp in
      let results = mapM (env.courseStore.getCourse) $ Set.toList courseIds in
      case results of
        Just courses ->
          courses
        Nothing ->
          error "Not all courses could be retrieved"


isActive :: Module -> ISP -> Bool
isActive mod isp =
  let StudyProgram.ModuleActivator f = mod.commonFields.activator in
     f $ isp.options
--
checkModule :: Module -> ModuleChecker
checkModule mod = do
  env <- ask
  if isActive mod env.isp
  then do
    let scope = getScope mod env.isp
    -- Not only applies checkConstraint to each constraint in the list, but also sequences the results in a single monadic action that
    -- Produces all results. If at least one result returned Nothing, the binding fails and checkModule will return Nothing as well.
    subModuleResults <- mapM checkModule (mod.subModules)
    constraintResults <- mapM (\c -> checkConstraint (Constraints.ScopedConstraint c scope)) (mod.commonFields.constraints)
    let allResults = subModuleResults ++ results
    let isFail = Set.member False $ Set.fromList $ map toBool allResults
    if isFail then
      let errorMsg = pack $ "Module " ++ mod.commonFields.name ++ " voldoet niet aan vereisten." in
        return $ ModuleFail { errorMsg, subModuleResults, constraintResults }
    else
      return ModuleSuccess
    -- You provide a function that maps a result to a boolean. Then provide a list of results. You get a boolean if all the outcomes of the results are booleans.
--    return $ all id $ Set.toList $ Set.union (Set.fromList subModuleResults) (Set.fromList results) -- (all :: (a -> Bool) -> [a] -> Bool. First argument is the predicate (in this case id function, because results are already booleans)
  else return ModuleSuccess
--
checkConstraint :: Constraints.Constraint -> ConstraintChecker

checkConstraint (Constraints.ModuleConstraint desc c) = do
  checkConstraint c -- Description is irrelevant for constraint checking

checkConstraint (Constraints.IncludedConstraint code) = do
  env <- ask
  let res = Set.member code $ ISP.getIncludedCourses env.isp
  if res then
    return ConstraintSuccess
  else
    let errorMsg = pack $ "Het vak '" ++ code ++ "' is niet opgenomen in het ISP." in
      return $ ConstraintFail {errorMsg, subResults=[]}

checkConstraint (Constraints.NandConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = not (toBool r1 && toBool r2)
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

checkConstraint (Constraints.AndConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = toBool r1 && toBool r2
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

checkConstraint (Constraints.OrConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = toBool r1 || toBool r2
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

checkConstraint (Constraints.NorConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = not (toBool r1 || toBool r2)
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

checkConstraint (Constraints.XorConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = (toBool r1 || toBool r2) && toBool r1 /= toBool r2
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

checkConstraint (Constraints.NotConstraint c) = do
  r <- checkConstraint c -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  let res = not $ toBool r
  return $ if res then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[]}

checkConstraint (Constraints.MinSPConstraint sp) = do
  env <- ask
  let courses = ConstraintChecker.getCourses env
  let totalSP = sum $ map (\c -> c.studyPoints) courses
  let r = (totalSP >= sp) -- Return now maps the boolean inside a maybe monad, wraps it inside the ReaderT
  if r then
    return ConstraintSuccess
  else
    let errorMsg = pack $ "Minimum aantal studiepunten in module moet " ++ show sp ++ " zijn, en is " ++ show totalSP in
      return $ ConstraintFail {errorMsg, subResults=[]}
--  lift Nothing -- Return would wrap the result of 'lift Nothing' in another layer of ReaderT

checkConstraint (Constraints.MaxSPConstraint sp) = do
  env <- ask
  let courses = ConstraintChecker.getCourses env
  let totalSP = sum $ map (\c -> c.studyPoints) courses
  let r = (totalSP <= sp) -- Return now maps the boolean inside a maybe monad, wraps it inside the ReaderT
  if r then
    return ConstraintSuccess
  else
    let errorMsg = pack $ "Maximum aantal studiepunten in module moet " ++ show sp ++ " zijn, en is " ++ show totalSP in
      return $ ConstraintFail {errorMsg, subResults=[]}
--
---- Const is a function that ignores its argument and returns a constant value.
---- We require const here because local expects a function that takes in an env and returns an adjusted environment.
---- But we have already created the newEnv via different variable, so we can just return that value.
checkConstraint (Constraints.ScopedConstraint constraint newScope) = do
  env <- ask
  let newISP = filterISP (env.isp) newScope
  let newEnv = env { isp = newISP }
  local (const newEnv) (checkConstraint constraint)

checkConstraint (Constraints.SameYearConstraint code1 code2) = do
  env <- ask
  let plannedPerYear = ISP.getPlannedPerYear $ env.isp.courseSelection in
    let setsContainingBoth = filter (\s -> Set.member code1 s && Set.member code2 s) plannedPerYear in
      -- Either they are simply not included, or both are included in that year
      if (length setsContainingBoth == 1 || length setsContainingBoth == 0) then
        lift Nothing -- TODO implement same year constraint
      else
        lift Nothing -- TODO implement same year constraint

getScope :: Module -> ISP -> Constraints.Scope
getScope mod isp =
  if isActive mod isp
  then
    let coursesInMod = Set.fromList mod.commonFields.courses in -- TODO needs to get sub modules as well
    let courses = foldr (\mod acc -> Set.union (getScope mod isp) acc) coursesInMod mod.subModules in
      courses

--  then Set.union (courses mod) ++ concatMap (\subMod -> getScope subMod isp) (subModules mod)
  else Set.empty
--
filterISP :: ISP -> Constraints.Scope -> ISP
filterISP isp scope =
  let
  courseSel = isp.courseSelection
  passed = courseSel.passed
  planned = courseSel.planned in
    let
    filteredPassed = Set.intersection scope passed
    filteredPlanned = map (Set.intersection scope) planned in
      isp { ISP.courseSelection = ISP.CourseSelection { ISP.passed = filteredPassed, ISP.planned = filteredPlanned } }

runCheckModule :: Module -> Env -> Maybe CCResult
runCheckModule mod env = runReaderT (checkModule mod) env