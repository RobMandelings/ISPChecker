module Constraints where

import Courses
import qualified Data.Set as Set
import Data.Text (Text)

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

type Scope = Set.Set CourseCode

-- TODO implement the PeriodConstraint: course can only be selected in proper period. Independent of the study program but still a constraint I think.
--  (additional constraint can be added to studyprogram? E.g. in this program you need to do it in sem 2, even though the course is available in sem 1 as well)

data Constraint =
  SomeConstraint Constraint | -- There is some course for which the following constraint holds
  AllConstraint CourseCode Constraint | -- For all courses the following constraints hold (such as AllIncluded)
  IncludedConstraint CourseCode |
  NotConstraint Constraint |
  NandConstraint Constraint Constraint |
  AndConstraint Constraint Constraint |
  OrConstraint Constraint Constraint |
  NorConstraint Constraint Constraint |
  XorConstraint Constraint Constraint |
  MinSPConstraint Int |
  MaxSPConstraint Int |
  SameYearConstraint CourseCode CourseCode |
  ScopedConstraint Constraint Scope | -- Nested constraint only applies to given scope
  ModuleConstraint Text Constraint -- ModuleConstraint essentially wraps a constraint and adds a description for this constraint.
  deriving (Show, Generic)

-- Used to convert any constraint into json format, to be used in the front-end
instance Aeson.ToJSON Constraint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- A built-in constraint combinator to combine both the MinSPConstraint with the MaxSPConstraint
rangeSPConstraint :: Int -> Int -> Constraint
rangeSPConstraint minSP maxSP = AndConstraint (MinSPConstraint minSP) (MaxSPConstraint maxSP)