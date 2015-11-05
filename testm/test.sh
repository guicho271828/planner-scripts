#!/bin/bash

../cgroup-setup.sh

## mp
# this should finish normally (because it is easy)
../clean.sh
../limit.sh m-clean p01.pddl domain.pddl

../limit.sh -t 1 -- m-clean p01.pddl domain.pddl

../limit.sh -t 1 -- m-clean p18.pddl domain.pddl

# increased verbosity
../limit.sh -t 1 -v -- m-clean p01.pddl domain.pddl


# debugging: do not remove temporary directory
../limit.sh -t 1 -d -- m-clean  p03.pddl domain.pddl

# what happens when there are no solution?
../limit.sh -t 1 -v -- m-clean unsolvable.pddl domain.pddl

