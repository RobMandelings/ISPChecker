module Activator where

import Control.Monad.Reader
import qualified ISP

data ActivatorVal =
  ISPRef String |
  Literal String

data ActivatorConstraint =
  ComparisonConstraint ActivatorVal ActivatorVal |
  NotConstraint ActivatorConstraint |
  NandConstraint ActivatorConstraint ActivatorConstraint |
  AndConstraint ActivatorConstraint ActivatorConstraint |
  OrConstraint ActivatorConstraint ActivatorConstraint |
  NorConstraint ActivatorConstraint ActivatorConstraint |
  XorConstraint ActivatorConstraint ActivatorConstraint

data Env = Env
  {
  isp :: ISP.ISP -- The ISP that was parsed
  }

type ActivationChecker = ReaderT Env Maybe Bool

checkActive :: ActivatorConstraint -> ActivationChecker

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