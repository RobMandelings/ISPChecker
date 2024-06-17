module StudyProgram where

import Data.Map.Strict as Map

import qualified Courses
import Constraints
import ISP

newtype ModuleActivator = ModuleActivator (ISPOptions -> Bool)

-- If no custom activator is chosen, the module will be active by default
trueActivator :: ModuleActivator
trueActivator = ModuleActivator $ \options -> True

optionAndNameActivator :: String -> String -> ModuleActivator
optionAndNameActivator optionName optionValue =
  ModuleActivator $ \options -> case Map.lookup optionName options of
                                  Just val -> val == optionValue
                                  Nothing -> False

specActivator :: String -> ModuleActivator
specActivator spec = optionAndNameActivator "specialisation" spec

-- Module Activator depending on background education
backgroundEduActivator :: String -> ModuleActivator
backgroundEduActivator bgEdu = optionAndNameActivator "backgroundEducation" bgEdu

class ModuleCommon a where
  getName :: a -> String
  getDescription :: a -> String
  getCourses :: a -> [Courses.CourseCode]
  getConstraints :: a -> [Constraint]
  getActivator :: a -> ModuleActivator

data ModuleCommonFields = ModuleCommonFields
  { name :: String,
    description :: String,
    courses :: [Courses.CourseCode],
    constraints :: [Constraint],
    activator :: ModuleActivator
  } deriving (Show)

data Module = Module
  {
  commonFields :: ModuleCommonFields,
  subModules :: [Module]
  } deriving (Show)

data ModuleWRef = ModuleWRef
  {
  commonFields :: ModuleCommonFields,
  subModules :: [Either String ModuleWRef]
  }

instance ModuleCommon ModuleWRef where
 getName ModuleWRef{..} = commonFields.name
 getDescription ModuleWRef{..} = commonFields.description
 getCourses ModuleWRef{..} = commonFields.courses
 getConstraints ModuleWRef{..} =  commonFields.constraints
 getActivator ModuleWRef{..} = commonFields.activator

instance ModuleCommon Module where
 getName Module{..} = commonFields.name
 getDescription Module{..} = commonFields.description
 getCourses Module{..} = commonFields.courses
 getConstraints Module{..} = commonFields.constraints
 getActivator Module{..} = commonFields.activator


instance Show ModuleActivator where
  show _ = show "<ModuleActivator>"

--instance Show Module where
--  show (Module name description courses constraints subModules _) =
--    "Module {\nname = " ++ show name ++
--    ", \ndescription = " ++ show description ++
--    ", \ncourses = " ++ show courses ++
--    ", \nsubModules = " ++ show subModules
