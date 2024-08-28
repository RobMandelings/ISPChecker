module Preparation where

  import qualified Data.Map as Map
  import qualified StudyProgram
  import Control.Monad.Reader
  import Control.Monad ( (>=>))

  -- | The parser is able to handle references to modules that are defined somewhere in the file.
  -- I need to make sure that these references are dealt with when checking a modules
  data Env = Env { modMap :: (Map.Map String StudyProgram.ModuleWRef) }

  -- | Given a module or a reference to a module, return the actual module.
  -- | In case a module is given, nothing needs to happen, but in case a reference is provided, the module that is referenced needs to be returned.
  getModule :: Either String StudyProgram.ModuleWRef -> Reader Env StudyProgram.ModuleWRef
  getModule mod = do
    env <- ask
    case mod of
      Left id -> case Map.lookup id env.modMap of {
          Just modWRef -> return modWRef;
          Nothing -> error $ "No module with id " ++ (show id) ++ " found"
        }
      Right modWRef -> return modWRef

  -- | Given a module that might contain references, build the module by resolving the references.
  buildModule :: StudyProgram.ModuleWRef -> Reader Env StudyProgram.Module
  buildModule mod = do
    env <- ask
    subModules <- mapM (getModule >=> buildModule) mod.subModules
    return $ StudyProgram.Module { commonFields = mod.commonFields, subModules }

  -- | Given a map of modules, build all modules by resolving the references within each module.
  buildModules :: Map.Map String StudyProgram.ModuleWRef -> Reader Env (Map.Map String StudyProgram.Module)
  buildModules modMap = do
    env <- ask
    let mods = Map.mapWithKey (\name modWRef -> runReader (buildModule modWRef) env) modMap
    return mods