module Preparation where

  import qualified Data.Map as Map
  import qualified StudyProgram

  -- | TODO optimization: only retrieve references to modules when necessary (i.e. if module is activated). Otherwise there is no need to retrieve reference.
  -- For now we will not handle this case yet.

  -- | The parser is able to handle references to modules that are defined somewhere in the file.
  -- I need to make sure that these references are dealt with when checking a modules

  buildModule :: StudyProgram.Module -> Map.Map String StudyProgram.Module -> StudyProgram.Module
  buildModule = error "Not implemented yet"

