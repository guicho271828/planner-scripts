#! /bin/bash

## fastdownward
# this should finish normally (because it is easy)
../clean.sh
../test-problem.sh p0001.pddl domain.pddl

../clean.sh
../test-problem.sh -t 1 p0001.pddl domain.pddl

# this should fail (because soft > hard)
../clean.sh
../test-problem.sh -T 1 p0001.pddl domain.pddl

../clean.sh
../test-problem.sh -t 1 -T 30 p0002.pddl domain.pddl

# force hard limit
../clean.sh
../test-problem.sh -t 3 -T 5 p0004.pddl domain.pddl

# increased verbosity
../clean.sh
../test-problem.sh -v -t 1 p0001.pddl domain.pddl
