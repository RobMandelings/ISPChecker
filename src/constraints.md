## Aggregates
- All
- List notation […] (specific courses)

### Aggregate constraint
- Aggregates by themselves only make a selection of courses, they don’t have any meaning with regard to requirements yet.
- Precede aggregate with some operator
    - &[…] or |[…]
    - Unless they can be combined with the normal AND, OR, … operators somehow?

- None
    - ![]

- Main difference: element-wise or global?

## SP Constraints
- Range (sp1, sp2)
- Min (sp)
- Max (sp)
- Remaining (c, sp)
    - Imposes that the course can only be selected in the ISP once there are only 72 remaining SP.

## Logical
- Nand (c1, c2)
- Xor
- Not
- Or

## Period
- Sameperiod(c1, c2) = period (c1) == period(c2)