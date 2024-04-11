module StudyProgram where

import Data.Map.Strict as Map

import Courses
import Constraints
import ISP

type ModuleActivator = ISPOptions -> Bool

-- If no custom activator is chosen, the module will be active by default
trueActivator :: ModuleActivator
trueActivator ispOptions = True

optionAndNameActivator :: String -> String -> ModuleActivator
optionAndNameActivator optionName optionValue ispOptions =
  case Map.lookup optionName ispOptions of
    Just val -> val == optionValue
    Nothing -> False

specActivator :: String -> ModuleActivator
specActivator spec ispOptions = optionAndNameActivator "specialisation" spec ispOptions

-- Module Activator depending on background education
backgroundEduActivator :: String -> ModuleActivator
backgroundEduActivator bgEdu ispOptions = optionAndNameActivator "backgroundEducation" bgEdu ispOptions

data Module = Module
  { name :: String
  , courses :: [Course]
  , constraints :: [Constraint]
  , subModules :: [Module]
  , moduleActivator :: ModuleActivator
  }

