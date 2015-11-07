#!/bin/bash

. ../test-common.sh

../cgroup-setup.sh

test-non-ff (){
    # this should finish normally (because it is easy)
    ../clean.sh

    success ../limit.sh $1 p01.pddl domain.pddl

    success ../limit.sh -t 1 -- $1 p01.pddl domain.pddl

    fail ../limit.sh -t 1 -- $1 p22.pddl domain.pddl

    # # increased verbosity
    # success ../limit.sh -t 1 -v -- $1 p01.pddl domain.pddl
    # 
    # # debugging: do not remove temporary directory
    # success ../limit.sh -t 1 -d -- $1  p03.pddl domain.pddl

    # what happens when there are no solution?
    fail ../limit.sh -t 1 -v -- $1 unsolvable.pddl domain.pddl

    success test -e unsolvable.negative
}


for planner in ff lpg marvin1 marvin2 mff mpc mp m probe
do
    test-non-ff $planner-clean
done


fail ../limit.sh -t 1 -v -- marvin1-clean tidy-p01.pddl tidy-domain.pddl

