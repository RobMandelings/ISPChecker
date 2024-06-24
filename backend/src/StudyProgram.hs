module StudyProgram where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Data.Map.Strict as Map

import qualified Courses
import Constraints
import ISP
import qualified Data.Text as T

newtype ModuleActivator = ModuleActivator (ISPOptions -> Bool)

data ModuleCommonFields = ModuleCommonFields
  { name :: String,
    description :: String,
    courses :: [Courses.CourseCode],
    constraints :: [Constraint],
    activator :: ModuleActivator
  } deriving (Show, Generic)

data Module = Module
  {
  commonFields :: ModuleCommonFields,
  subModules :: [Module]
  } deriving (Show, Generic)

data ModuleWRef = ModuleWRef
  {
  commonFields :: ModuleCommonFields,
  subModules :: [Either String ModuleWRef]
  } deriving (Show, Generic)


instance Show ModuleActivator where
  show _ = show "<ModuleActivator>"

instance Aeson.ToJSON ModuleActivator where
  toJSON act = Aeson.String $ T.pack $ show act
--  ModuleActivator does not have a generic encoding
--  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON ModuleCommonFields where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON Module where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON ModuleWRef where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

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

--instance Show Module where
--  show (Module name description courses constraints subModules _) =
--    "Module {\nname = " ++ show name ++
--    ", \ndescription = " ++ show description ++
--    ", \ncourses = " ++ show courses ++
--    ", \nsubModules = " ++ show subModules
