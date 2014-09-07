#!/bin/bash

../cgroup-setup.sh

## ff
# this should finish normally (because it is easy)
../clean.sh
../limit.sh ../marvin-clean p01.pddl domain.pddl

../clean.sh
../limit.sh -t 1 -- ../marvin-clean p01.pddl domain.pddl

../clean.sh
../limit.sh -t 1 -- ../marvin-clean p03.pddl domain.pddl

# increased verbosity
../clean.sh
../limit.sh -t 1 -- ../marvin-clean -v p01.pddl domain.pddl

# what happens when there are no solution?
../clean.sh
../limit.sh -t 1 -- ../marvin-clean -v unsolvable.pddl domain.pddl

