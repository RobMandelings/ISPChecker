module Preparation where

  import qualified Data.Map as Map
  import qualified StudyProgram
  import Control.Monad.Reader
  import Control.Monad ( (>=>))

  -- | TODO optimization: only retrieve references to modules when necessary (i.e. if module is activated). Otherwise there is no need to retrieve reference.
  -- For now we will not handle this case yet.

  -- | The parser is able to handle references to modules that are defined somewhere in the file.
  -- I need to make sure that these references are dealt with when checking a modules

  data Env = Env { modMap :: (Map.Map String StudyProgram.ModuleWRef) }

  getModule :: Either String StudyProgram.ModuleWRef -> Reader Env StudyProgram.ModuleWRef
  getModule mod = do
    env <- ask
    case mod of
      Left id -> case Map.lookup id env.modMap of {
          Just modWRef -> return modWRef;
          Nothing -> error $ "No module with id " ++ (show id) ++ " found"
        }
      Right modWRef -> return modWRef

  buildModule :: StudyProgram.ModuleWRef -> Reader Env StudyProgram.Module
--  buildModule = do
  buildModule mod = do
    env <- ask
    subModules <- mapM (getModule >=> buildModule) mod.subModules
    return $ StudyProgram.Module { commonFields = mod.commonFields, subModules = [] }
