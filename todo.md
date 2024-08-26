# TODO

- Aggregate constraints: for all courses: included (course) for example
    - For some course: included (course)

- Defining scopes

- Prerequisite courses

- Activation of modules (using module activator)
    - Write the appropriate DSL interpretation for it

- Write the documentation
- Write the report

## Optional

- References to ModuleConstraints instead of inline definitions (with errormsg, description and everything)

## To put in the report

- The use of heterogeneous lists to parse any argument in any order
- Explain the reasoning behind the activation of modules (function vs data structure, why?)

- Encountered problem where submodule is inactive (itself does not need to check for restrictions), but the upper level
  does need to check for all courses (with minSP) (and how I solved it)

### Possible improvements in the future

- Consolidate both the normal constraint and activator constraint parsing in some way (because they both have unary and
  binary constraints)

- Vrijstellingen
- Remove the redundant komma at the end of an object definition
- Onnodige volgorde bij definities
- Better reference handling
- Ability to use databases to get the course data (is now all present in the file which is not optimal)
- Editors for the begeleiders (real front end) to edit the study programs.

- Semantische analyse voor betere programmatie

- Explain why you chose to do the data constructor for activator instead of a function from ISPOptions to a boolean