#!/bin/bash

../cgroup-setup.sh

## ff
# this should finish normally (because it is easy)
../clean.sh
../limit.sh macroff-clean p01.pddl domain.pddl

../limit.sh -t 1 -- macroff-clean p01.pddl domain.pddl

../limit.sh -t 1 -- macroff-clean p18.pddl domain.pddl

# increased verbosity
../limit.sh -t 1 -v -- macroff-clean p01.pddl domain.pddl


# debugging: do not remove temporary directory
../limit.sh -t 1 -d -- macroff-clean  p03.pddl domain.pddl

# what happens when there are no solution?
../limit.sh -t 1 -v -- macroff-clean unsolvable.pddl domain.pddl

