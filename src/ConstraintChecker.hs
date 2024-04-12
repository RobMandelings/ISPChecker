module ConstraintChecker where

import Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Reader

import Courses
import Constraints
import ISP

data Env = Env
  { isp :: ISP
  , scope :: Scope
  }

type ConstraintChecker = ReaderT Env Maybe

--checkConstraint :: Constraint -> ConstraintChecker Bool
--checkConstraint (MinSPConstraint sp) = do
--  scope <- asks scope
--  let courses = map (Map.lookup)
--
--  let totalSP = sum $ map (studyPoints) scope
--  return (totalSP >= sp)

-- Returns the corresponding
getCoursesWithYearFromCodes :: [CourseCode] -> ISP -> [CourseWithYear]
getCoursesWithYearFromCodes codes isp = Data.Maybe.mapMaybe (`Map.lookup` (courses isp)) codes