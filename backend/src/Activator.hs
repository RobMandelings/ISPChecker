module Activator where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Control.Monad.Reader
import Debug.Trace
import qualified ISP

-- | Each module may optionally have an activator constraint. This constraint is evaluated at runtime given a filled-in ISP.
-- A module activation can be composed of several activation 'functions', allowing for complex activations if necessary.
data ActivatorConstraint =
  TrueConstraint |
  EqualConstraint String String |
  NotConstraint ActivatorConstraint |
  NandConstraint ActivatorConstraint ActivatorConstraint |
  AndConstraint ActivatorConstraint ActivatorConstraint |
  OrConstraint ActivatorConstraint ActivatorConstraint |
  NorConstraint ActivatorConstraint ActivatorConstraint |
  XorConstraint ActivatorConstraint ActivatorConstraint
  deriving (Show, Generic)

-- | Used to convert the activator constraint into json format for front-end parsing.
instance Aeson.ToJSON ActivatorConstraint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | Environment used for the reader to check whether a module is activated
data Env = Env
  {
  isp :: ISP.ISP -- The ISP that was parsed
  }

-- | Monadic type returned after evaluating whether a module is activated.
type ActivationChecker = ReaderT Env Maybe Bool

-- | given an activator constraint, returns an activation checker. Uses the environment to evaluate the constraint.
checkActive :: ActivatorConstraint -> ActivationChecker

-- | Trivial activator constraint. Modules with this constraint are always activated (default for modules)
checkActive (TrueConstraint) = do
  return True

-- | Checks whether the given ISP option (identifier is on the left hand side) is equal to the corresponding value.
checkActive (EqualConstraint lhs rhs) = do
  env <- ask
  let optionVal = ISP.getOption env.isp lhs -- The value of the ISP option (e.g. specialisation or background)d
--  trace ("isActive? " ++ show optionVal ++ " vs " ++ show lhs) $
  return $ optionVal == rhs -- Checks whether the value of the ISP option is equal to the right hand side

checkActive (NandConstraint c1 c2) = do
  r1 <- checkActive c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkActive c2
  let r = not (r1 && r2)
  return $ if r then True else False

checkConstraint (AndConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = r1 && r2
  return $ if r then True else False

checkConstraint (OrConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = r1 || r2
  return $ if r then True else False

checkConstraint (NorConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = not (r1 || r2)
  return $ if r then True else False

checkConstraint (XorConstraint c1 c2) = do
  r1 <- checkConstraint c1 -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  r2 <- checkConstraint c2
  let r = (r1 || r2) && r1 /= r2
  return $ if r then True else False

checkConstraint (NotConstraint c) = do
  r <- checkConstraint c -- If checkConstraint returns Nothing, the do block short-circuits and Nothing is returned instead. If it returns Just x, then x is binded to r1. With let r1 = checkConstraint ... we don't extract x.
  let res = not $ r
  return $ if r then True else False