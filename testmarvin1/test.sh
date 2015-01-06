#!/bin/bash

../cgroup-setup.sh

## ff
# this should finish normally (because it is easy)
../clean.sh
../limit.sh -- marvin1-clean p01.pddl domain.pddl

../limit.sh -t 1 -- marvin1-clean p01.pddl domain.pddl

../limit.sh -t 1 -- marvin1-clean p18.pddl domain.pddl

# increased verbosity
../limit.sh -t 1 -v -- marvin1-clean p01.pddl domain.pddl


# debugging: do not remove temporary directory
../limit.sh -t 1 -d -- marvin1-clean  p03.pddl domain.pddl

# debugging: test marvin -l option
../limit.sh -t 1 -d -o "-l" -- marvin1-clean p03.pddl domain.pddl

# what happens when there are no solution?
../limit.sh -t 1 -v -- marvin1-clean unsolvable.pddl domain.pddl



# typed domain
../limit.sh -t 1 -v -- marvin1-clean tidy-p01.pddl tidy-domain.pddl

