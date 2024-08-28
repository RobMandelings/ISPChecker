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
import qualified Activator

import qualified Courses
import StudyProgram
import qualified Constraints
import ISP (ISP)
import qualified ISP as ISP
import Debug.Trace
import Text.Show.Pretty (ppShow)
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

-- | ToBool typeclass is used to convert a type to a boolean value
class ToBool a where
  toBool :: a -> Bool

-- | ConstraintResult is used to store the result of a constraint check
-- Either successful, no extra information is needed
-- Or failed, in which case the error message and sub constraint results are stored
data ConstraintResult =
  ConstraintSuccess |
  ConstraintFail {
    errorMsg :: Text,
    subResults :: [ConstraintResult]
  } deriving (Show, Generic)

-- | ModuleResult is used to store the result of a module check
-- Either successful, no extra information is needed
-- Or failed, in which case the constraint results and sub module results are stored
data ModuleResult =
  ModuleSuccess {
    subModuleResults :: [ModuleResult]
  } |
  ModuleInactive |
  ModuleFail {
    constraintResults :: [ConstraintResult],
    subModuleResults :: [ModuleResult]
  } deriving (Show, Generic)

-- | Instance of TypeClass to convert a ConstraintResult type into Json format
instance Aeson.ToJSON ConstraintResult where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | Instance of TypeClass to convert a ModuleResult type into Json format
instance Aeson.ToJSON ModuleResult where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | Instance to convert ModuleResult to their boolean value
instance ToBool ModuleResult where
  toBool (ModuleSuccess _) = True
  toBool ModuleInactive = True
  toBool (ModuleFail _ _) = False -- TODO cleaner way to handle this?

-- | Instance to convert ConstraintResult to their boolean value
instance ToBool ConstraintResult where
  toBool ConstraintSuccess = True
  toBool (ConstraintFail _ _) = False

-- | CourseStore type where a function getCourse can be provided. This is because there might be different implementations of this function that should be abstracted. (e.g. database vs in-memory)
data CourseStore = CourseStore { getCourse :: Courses.CourseCode -> Maybe Courses.Course }

-- | Maps the course code to the corresponding course
type CourseMap = Map.Map Courses.CourseCode Courses.Course

-- | Creates a CourseStore based on a Map implementation
createMapCourseStore :: CourseMap -> CourseStore
createMapCourseStore courseMap =
  CourseStore { getCourse = (\courseCode -> Map.lookup courseCode courseMap) }

-- | Creates a CourseStore based on a DB implementation
createDBCourseStore :: String -> CourseStore
createDBCourseStore dbName =
  CourseStore { getCourse = (\courseCode -> Nothing) }

-- | Environment used to run the constraint checker.
data Env = Env
  {
    scope :: Constraints.Scope
  , isp :: ISP -- The ISP that was parsed
  , courseStore :: CourseStore
  }

-- | Monad used to run the constraint checker
type ModuleChecker = ReaderT Env Maybe ModuleResult

-- | Monad used to run the constraint checker
type ConstraintChecker = ReaderT Env Maybe ConstraintResult

-- | Retrieves a list of courses that are included in the ISP (not the codes, but actual courses)
getCourses :: Env -> [Courses.Course]
getCourses env =
    let courseIds = ISP.getIncludedCourses $ env.isp in
      let results = mapM (env.courseStore.getCourse) $ Set.toList courseIds in
      case results of
        Just courses ->
          courses
        Nothing ->
          error "Not all courses could be retrieved"

-- | Returns whether a Module is activated based on settings within the ISP
isActive :: Module -> ISP -> Bool
isActive mod isp =
  let env = Activator.Env { isp = isp } in
  let res = runReaderT (Activator.checkActive mod.commonFields.activator) env in
    case res of
      Just val ->
        val
      Nothing ->
        error "Something went wrong while checking whether module is active"

-- | Checks the constraints of a module (and all its submodules) for constraint violations.
checkModule :: Module -> ModuleChecker
checkModule mod = do
  env <- ask
   -- If the module is activated, we need to check for constraints. E.g. module 'A.I' is activated, so needs to be checked.
   -- Module 'Distributed Systems' is deactivated, does not need to be checked (automatic success)
  if isActive mod env.isp
  then do
    let scope = getScope mod env.isp
    -- Not only applies checkConstraint to each constraint in the list, but also sequences the results in a single monadic action that
    -- Produces all results. If at least one result returned Nothing, the binding fails and checkModule will return Nothing as well.
    subModuleResults <- mapM checkModule (mod.subModules)
    constraintResults <- mapM (\c -> checkConstraint (Constraints.ScopedConstraint c scope)) (mod.commonFields.constraints)
    let subModuleFail = Set.member False $ Set.fromList $ map toBool subModuleResults
    let constraintFail = Set.member False $ Set.fromList $ map toBool constraintResults
    if subModuleFail || constraintFail then
        return $ ModuleFail { subModuleResults, constraintResults }
    else
      return ModuleSuccess { subModuleResults }
    -- You provide a function that maps a result to a boolean. Then provide a list of results. You get a boolean if all the outcomes of the results are booleans.
--    return $ all id $ Set.toList $ Set.union (Set.fromList subModuleResults) (Set.fromList results) -- (all :: (a -> Bool) -> [a] -> Bool. First argument is the predicate (in this case id function, because results are already booleans)
  else return ModuleInactive
--

-- | Replace a course code reference to an actual course code (the reference acts as a placeholder)
-- | Used when the constrains have been quanfied.
replaceCourseCodeRef :: Courses.CourseCode -> Constraints.Constraint -> Courses.CourseCode -> Constraints.Constraint
replaceCourseCodeRef codeRef constraint newCode =
  case constraint of
    Constraints.IncludedConstraint code ->
      if code == codeRef then
        Constraints.IncludedConstraint newCode
      else
        constraint
    Constraints.NandConstraint c1 c2 -> Constraints.NandConstraint (replaceCourseCodeRef codeRef c1 newCode) (replaceCourseCodeRef codeRef c2 newCode)
    Constraints.AndConstraint c1 c2 -> Constraints.AndConstraint (replaceCourseCodeRef codeRef c1 newCode) (replaceCourseCodeRef codeRef c2 newCode)
    Constraints.OrConstraint c1 c2 -> Constraints.OrConstraint (replaceCourseCodeRef codeRef c1 newCode) (replaceCourseCodeRef codeRef c2 newCode)
    Constraints.NorConstraint c1 c2 -> Constraints.NorConstraint (replaceCourseCodeRef codeRef c1 newCode) (replaceCourseCodeRef codeRef c2 newCode)
    Constraints.XorConstraint c1 c2 -> Constraints.XorConstraint (replaceCourseCodeRef codeRef c1 newCode) (replaceCourseCodeRef codeRef c2 newCode)
    Constraints.NotConstraint c -> Constraints.NotConstraint (replaceCourseCodeRef codeRef c newCode)
    _ ->
      constraint

-- | Checks a single constraint.
checkConstraint :: Constraints.Constraint -> ConstraintChecker

-- | Checks a module constraint
checkConstraint (Constraints.ModuleConstraint desc c) = do
  checkConstraint c -- Description is irrelevant for constraint checking, so just return the result of the nested constraint.

-- | Checks whether for all courses in the current scope, the constraint holds (e.g. for all courses: Included(course)
checkConstraint (Constraints.AllConstraint codeRef constraint) = do
  env <- ask
  -- Map every course code in the scope to a constraint that replaces the code reference (the placeholder course) with the course code
  -- This yields a list of constraints where in each constraint, the placeholder has been replaced by a specific course code.
  let results = map (replaceCourseCodeRef codeRef constraint) (Set.toList $ env.scope)
  constraintCheckResults <- mapM checkConstraint results

  -- If at least one of the constraint fails, then the parent constraint fails as well (AllConstraint)
  let fail = Set.member False $ Set.fromList $ map toBool constraintCheckResults
  if fail then
    let errorMsg = pack $ "Niet alle vakken voldoen aan de vereisten" in
      return $ ConstraintFail { errorMsg, subResults=constraintCheckResults }
  else
    return ConstraintSuccess


-- | Checks whether a courseCode is included in the ISP
checkConstraint (Constraints.IncludedConstraint code) = do
  env <- ask
  let res = Set.member code $ ISP.getIncludedCourses env.isp
  if res then
    return ConstraintSuccess
  else
    let errorMsg = pack $ "Het vak '" ++ code ++ "' is niet opgenomen in het ISP." in
      return $ ConstraintFail {errorMsg, subResults=[]}

-- | Checks whether two constraints are not simultaneously satisfied
checkConstraint (Constraints.NandConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = not (toBool r1 && toBool r2)
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

-- | Checks whether two constraints are simultaneously satisfied
checkConstraint (Constraints.AndConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = toBool r1 && toBool r2
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

-- | Checks whether one of the constraints are satisfied
checkConstraint (Constraints.OrConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = toBool r1 || toBool r2
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

-- | Checks whether none of the constraints are satisfied
checkConstraint (Constraints.NorConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = not (toBool r1 || toBool r2)
  return $ if r then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[r1, r2]}

-- | Checks whether exactly one of the constraints are satisfied
checkConstraint (Constraints.XorConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let res1 = (toBool r1 || toBool r2)
  let res = res1 && toBool r1 /= toBool r2 -- Checks for exclusion
  if res then
    return $ ConstraintSuccess
  else
    if not res1 then
      return $ ConstraintFail {errorMsg="Aan geen van beide condities is voldaan.", subResults=[r1, r2]}
    else
      return $ ConstraintFail {errorMsg="Het is niet toegelaten om aan beide voorwaarden tegelijkertijd te voldoen.", subResults=[r1, r2]}

-- | Checks whether a constraint is not satisfied
checkConstraint (Constraints.NotConstraint c) = do
  r <- checkConstraint c -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  let res = not $ toBool r
  return $ if res then ConstraintSuccess else ConstraintFail {errorMsg="", subResults=[]}

-- | Checks whether a minimum number of studypoints is included from the courses in the current scope
checkConstraint (Constraints.MinSPConstraint sp) = do
  env <- ask
  let courses = ConstraintChecker.getCourses env
  let totalSP = sum $ map (\c -> c.studyPoints) courses
  let r = (totalSP >= sp) -- Return now maps the boolean inside a maybe monad, wraps it inside the ReaderT
  if r then
    return ConstraintSuccess
  else
    let errorMsg = pack $ "Minimum aantal studiepunten moet " ++ show sp ++ " zijn, en is " ++ show totalSP in
      return $ ConstraintFail {errorMsg, subResults=[]}
--  lift Nothing -- Return would wrap the result of 'lift Nothing' in another layer of ReaderT

-- | Checks whether a maximum number of studypoints is included from the courses in the current scope
checkConstraint (Constraints.MaxSPConstraint sp) = do
  env <- ask
  let courses = ConstraintChecker.getCourses env
  let totalSP = sum $ map (\c -> c.studyPoints) courses
  let r = (totalSP <= sp) -- Return now maps the boolean inside a maybe monad, wraps it inside the ReaderT
  if r then
    return ConstraintSuccess
  else
    let errorMsg = pack $ "Maximum aantal studiepunten moet " ++ show sp ++ " zijn, en is " ++ show totalSP in
      return $ ConstraintFail {errorMsg, subResults=[]}

-- | Applies a scope of courseCodes to the nested constraints. This is implicit in modules, but they can also be explicitly defined in the DSL.
checkConstraint (Constraints.ScopedConstraint constraint newScope) = do
  env <- ask
  let newISP = filterISP (env.isp) newScope
  let newEnv = env { isp = newISP, scope = newScope }
  ---- Const is a function that ignores its argument and returns a constant value.
  ---- We require const here because local expects a function that takes in an env and returns an adjusted environment.
  ---- But we have already created the newEnv via different variable, so we can just return that value.
  local (const newEnv) (checkConstraint constraint)

-- | Checks whether two courses are included in the same year
checkConstraint (Constraints.SameYearConstraint code1 code2) = do
  env <- ask
  let passed = env.isp.courseSelection.passed in
    let plannedPerYear = ISP.getPlannedPerYear $ env.isp.courseSelection in
    let includedCourses = ISP.getIncludedCourses env.isp in
    let setsContainingBothPassed = (Set.member code1 passed && Set.member code2 passed) in
    let setsContainingBothPlanned = filter (\s -> Set.member code1 s && Set.member code2 s) plannedPerYear in

      if (not (Set.member code1 includedCourses) || not (Set.member code2 includedCourses)) then
        return ConstraintSuccess -- Rule does not apply if one (or both) courses are not included in the ISP
      -- Either they are simply not included, or both are included in that year
      else if ((not setsContainingBothPassed) && length setsContainingBothPlanned == 0) then
        return ConstraintFail { errorMsg="Vakken worden niet opgenomen in hetzelfde jaar", subResults=[] }
      else
        return ConstraintSuccess

-- | Gets all the courses that are included in this module. This includes all the courses that are in nested modules as well (recursively).
-- The scope of a module is used to check constraints at this level.
-- E.g. MinSP(120) means that at least 120SP should be included, and all included courses within the scope contribute to the SP value.
-- E.g. The scope of highest level module (the study program) contains all courses in the StudyProgram.
-- The isp is necessary to check for module activation.
getScope :: Module -> ISP -> Constraints.Scope
getScope mod isp =
  let coursesInMod = Set.fromList mod.commonFields.courses in -- Courses inside the module at current level

  -- Recursive fold function to get all courses that are in nested modules as well.
  let courses = foldr (\mod acc -> Set.union (getScope mod isp) acc) coursesInMod mod.subModules in
    courses

-- | Filters the ISP to the given scope. This affects functions like getIncludedCourses to only return courses that are within the scope.
-- | ISP's are therefore trimmed when they go down the tree of modules, as each module has a different scope (the top level module is the study program, where the scope is unaffected)
filterISP :: ISP -> Constraints.Scope -> ISP
filterISP isp scope =
  let
  courseSel = isp.courseSelection
  passed = courseSel.passed
  planned = courseSel.planned in
    let
    filteredPassed = Set.intersection scope passed -- Take the intersection of the new scope limit and the courses that are passed
    filteredPlanned = map (Set.intersection scope) planned in -- Since planned is a list of sets (because each set represents a semester), we need to map over this list to take the intersection
      isp { ISP.courseSelection = ISP.CourseSelection { ISP.passed = filteredPassed, ISP.planned = filteredPlanned } }

-- | Run the constraint checker for the given Module, in the given environment (where the ISP is defined).
-- | The module result is then a structure that provides information on which constraints are violated.
runCheckModule :: Module -> Env -> Maybe ModuleResult
runCheckModule mod env = runReaderT (checkModule mod) env