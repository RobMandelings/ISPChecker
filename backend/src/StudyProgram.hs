module StudyProgram where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Data.Map.Strict as Map

import qualified Activator
import qualified Courses
import Constraints
import ISP
import qualified Data.Text as T

-- | Both the Module and the ModuleWRef object has common fields. This is a separate object to avoid duplication of code.
data ModuleCommonFields = ModuleCommonFields
  { name :: String,
    description :: String,
    courses :: [Courses.CourseCode], -- Courses that this module adds (aside from the courses that are added by the submodules)
    constraints :: [Constraint], -- Constraints of this module (the scope to which these constraints apply is the courses of all submodules as well as the courses of this module)
    activator :: Activator.ActivatorConstraint -- Used to determine whether the module is activated or not. E.g. if there exists a module per specialisation, then only one of the modules should be activated.
    -- The others do not contribute in the constrain checker and the program can therefore not fail on these constraints.
  } deriving (Show, Generic)

-- | A module that only contains submodules and no references. This is the object that is used for constraint checking.
data Module = Module
  {
  commonFields :: ModuleCommonFields,
  subModules :: [Module]
  } deriving (Show, Generic)

-- | A module that might contain nested modules or simply references to other modules. This is what is parsed when the DSL code is read.
data ModuleWRef = ModuleWRef
  {
  commonFields :: ModuleCommonFields,
  subModules :: [Either String ModuleWRef]
  } deriving (Show, Generic)

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON ModuleCommonFields where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON Module where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | To convert the object into json format for usage in front-end
instance Aeson.ToJSON ModuleWRef where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions