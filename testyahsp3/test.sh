#!/bin/bash

../cgroup-setup.sh

## yahsp3
# this should finish normally (because it is easy)
../clean.sh
../limit.sh yahsp3-clean p01.pddl domain.pddl

../limit.sh -t 1 -- yahsp3-clean p01.pddl domain.pddl

../limit.sh -t 1 -- yahsp3-clean p18.pddl domain.pddl

# increased verbosity
../limit.sh -t 1 -v -- yahsp3-clean p01.pddl domain.pddl


# debugging: do not remove temporary directory
../limit.sh -t 1 -d -- yahsp3-clean  p03.pddl domain.pddl

# what happens when there are no solution?
../limit.sh -t 1 -v -- yahsp3-clean unsolvable.pddl domain.pddl

