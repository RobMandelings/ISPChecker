module Constraints where

data Constraint =
  NandConstraint Constraint Constraint

nandConstraint c1 c2 = NandConstraint c1 c2

andConstraint :: Constraint -> Constraint -> Constraint
andConstraint c1 c2 =
  let nand = nandConstraint c1 c2
  in notConstraint nand

notConstraint :: Constraint -> Constraint
notConstraint c1 = nandConstraint c1 c1

orConstraint :: Constraint -> Constraint -> Constraint
orConstraint c1 c2 = nandConstraint (notConstraint c1) (notConstraint c2)

norConstraint :: Constraint -> Constraint -> Constraint
norConstraint c1 c2 = notConstraint (orConstraint c1 c2)

xorConstraint :: Constraint -> Constraint -> Constraint
xorConstraint c1 c2 =
  let
    n1 = nandConstraint c1 c2
    n2 = nandConstraint c1 n1
    n3 = nandConstraint n1 c2
  in nandConstraint n2 n3