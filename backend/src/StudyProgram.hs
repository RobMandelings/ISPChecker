module StudyProgram where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Data.Map.Strict as Map

import qualified Activator
import qualified Courses
import Constraints
import ISP
import qualified Data.Text as T

data ModuleCommonFields = ModuleCommonFields
  { name :: String,
    description :: String,
    courses :: [Courses.CourseCode],
    constraints :: [Constraint],
    activator :: Activator.ActivatorConstraint
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

instance Aeson.ToJSON ModuleCommonFields where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON Module where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.ToJSON ModuleWRef where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

--instance Show Module where
--  show (Module name description courses constraints subModules _) =
--    "Module {\nname = " ++ show name ++
--    ", \ndescription = " ++ show description ++
--    ", \ncourses = " ++ show courses ++
--    ", \nsubModules = " ++ show subModules
