module Constraints where

data Constraint =
  NandConstraint Constraint Constraint |
  AndConstraint Constraint Constraint |
  OrConstraint Constraint Constraint

nandConstraint c1 c2 = NandConstraint c1 c2

andConstraint :: Constraint -> Constraint -> Constraint
andConstraint c1 c2 =
  let nand = nandConstraint c1 c2
  in nandConstraint nand nand

notConstraint :: Constraint -> Constraint
notConstraint c1 = nandConstraint c1 c1

orConstraint :: Constraint -> Constraint -> Constraint
orConstraint c1 c2 = nandConstraint (notConstraint c1) (notConstraint c2)