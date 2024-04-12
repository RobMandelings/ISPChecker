module ConstraintChecker where

import Control.Monad.Reader
import Constraints
import ISP

data Env = Env
  { isp :: ISP
  , scope :: Scope
  }

type ConstraintChecker = ReaderT Env Maybe