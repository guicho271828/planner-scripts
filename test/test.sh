#! /bin/bash


# this should finish normally (because it is easy)
../clean.sh
../test-problem.sh p0001.pddl domain.pddl

../clean.sh
../test-problem.sh -t 1 p0001.pddl domain.pddl

../clean.sh
../test-problem.sh -T 1 p0001.pddl domain.pddl

../clean.sh
../test-problem.sh -t 1 -T 30 p0002.pddl domain.pddl


