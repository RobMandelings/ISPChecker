module Activator where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Control.Monad.Reader
import Debug.Trace
import qualified ISP

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

instance Aeson.ToJSON ActivatorConstraint where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Env = Env
  {
  isp :: ISP.ISP -- The ISP that was parsed
  }

type ActivationChecker = ReaderT Env Maybe Bool

checkActive :: ActivatorConstraint -> ActivationChecker

checkActive (TrueConstraint) = do
  return True

checkActive (EqualConstraint lhs rhs) = do
  env <- ask
  let optionVal = ISP.getOption env.isp lhs
--  trace ("isActive? " ++ show optionVal ++ " vs " ++ show lhs) $
  return $ optionVal == rhs

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