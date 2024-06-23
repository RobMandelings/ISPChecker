module Constraints where

import Courses
import qualified Data.Set as Set

type Scope = Set.Set CourseCode

-- TODO implement the PeriodConstraint: course can only be selected in proper period. Independent of the study program but still a constraint I think.
--  (additional constraint can be added to studyprogram? E.g. in this program you need to do it in sem 2, even though the course is available in sem 1 as well)

data Constraint =
  IncludedConstraint CourseCode |
  NandConstraint Constraint Constraint |
  AndConstraint Constraint Constraint |
  NotConstraint Constraint |
  OrConstraint Constraint Constraint |
  NorConstraint Constraint Constraint |
  XorConstraint Constraint Constraint |
  MinSPConstraint Int |
  MaxSPConstraint Int |
  RemainingSPConstraint Int |
  SameYearConstraint CourseCode CourseCode |
  ScopedConstraint Constraint Scope -- Nested constraint only applies to given scope
  deriving (Show)

includedConstraint code = IncludedConstraint code
nandConstraint c1 c2 = NandConstraint c1 c2
minSPConstraint sp = MinSPConstraint sp
maxSPConstraint sp = MaxSPConstraint sp
sameYearConstraint code1 code2 = SameYearConstraint code1 code2
remainingSPConstraint sp = RemainingSPConstraint sp

-- Derived constraints

createAggregateAndConstraint :: [Constraint] -> Constraint
createAggregateAndConstraint constraints = undefined

rangeSPConstraint :: Int -> Int -> Constraint
rangeSPConstraint minSP maxSP = AndConstraint (MinSPConstraint minSP) (MaxSPConstraint maxSP)