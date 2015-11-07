#!/bin/bash

. ../test-common.sh

../cgroup-setup.sh

## ff
# this should finish normally (because it is easy)
../clean.sh

success ../limit.sh ff-clean p01.pddl domain.pddl

success ../limit.sh -t 1 -- ff-clean p01.pddl domain.pddl

fail ../limit.sh -t 1 -- ff-clean p22.pddl domain.pddl

# # increased verbosity
# success ../limit.sh -t 1 -v -- ff-clean p01.pddl domain.pddl
# 
# # debugging: do not remove temporary directory
# success ../limit.sh -t 1 -d -- ff-clean  p03.pddl domain.pddl

# what happens when there are no solution?
fail ../limit.sh -t 1 -v -- ff-clean unsolvable.pddl domain.pddl

success test -e unsolvable.negative
