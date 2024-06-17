{-# LANGUAGE RecordWildCards #-}

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

instance ModuleCommon Module where
 getName Module{..} = name commonFields
 getDescription Module{..} = description commonFields
 getCourses Module{..} = courses commonFields
 getConstraints Module{..} = constraints commonFields
 getActivator Module{..} = activator commonFields


instance Show ModuleActivator where
  show _ = show "<ModuleActivator>"

--instance Show Module where
--  show (Module name description courses constraints subModules _) =
--    "Module {\nname = " ++ show name ++
--    ", \ndescription = " ++ show description ++
--    ", \ncourses = " ++ show courses ++
--    ", \nsubModules = " ++ show subModules
