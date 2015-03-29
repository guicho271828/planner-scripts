#!/bin/bash

../cgroup-setup.sh

## ff
# this should finish normally (because it is easy)
../clean.sh
../limit.sh -- marvin2-clean p01.pddl domain.pddl

../limit.sh -t 1 -- marvin2-clean p01.pddl domain.pddl

../limit.sh -t 1 -- marvin2-clean p18.pddl domain.pddl

# increased verbosity
../limit.sh -t 1 -v -- marvin2-clean p01.pddl domain.pddl


# debugging: do not remove temporary directory
../limit.sh -t 1 -d -- marvin2-clean  p03.pddl domain.pddl

# debugging: test marvin2 -l option
../limit.sh -t 1 -d -o "-l" -- marvin2-clean p03.pddl domain.pddl

# what happens when there are no solution?
../limit.sh -t 1 -v -- marvin2-clean unsolvable.pddl domain.pddl

