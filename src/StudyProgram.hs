module StudyProgram where

import Courses
import Constraints

data Module = Module
  { name :: String
  , courses :: [Course]
  , constraints :: [Constraint]
  , subModules :: [Module]
  }