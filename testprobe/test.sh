#!/bin/bash

../cgroup-setup.sh

## probe
# this should finish normally (because it is easy)
../clean.sh
../limit.sh probe-clean p01.pddl domain.pddl

../limit.sh -t 1 -- probe-clean p01.pddl domain.pddl

../limit.sh -t 1 -- probe-clean p18.pddl domain.pddl

# increased verbosity
../limit.sh -t 1 -v -- probe-clean p01.pddl domain.pddl


# debugging: do not remove temporary directory
../limit.sh -t 1 -d -- probe-clean  p03.pddl domain.pddl

# what happens when there are no solution?
../limit.sh -t 1 -v -- probe-clean unsolvable.pddl domain.pddl

