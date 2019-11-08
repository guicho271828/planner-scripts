#!/bin/bash

. ../test-common.sh

# quick tests
../clean.sh

test-non-ff (){
    # this should finish normally (because it is easy)
    ../clean.sh

    section $1
    success ../limit.sh -- $1 -d p01.pddl domain.pddl
    success ../limit.sh -- $1 -d p02.pddl domain.pddl

    fail ../limit.sh -t 1 -- $1 -d p22.pddl domain.pddl
    fail test -e p22.negative

    # what happens when there are no solution?
    fail ../limit.sh -t 1 -- $1 -d unsolvable.pddl domain.pddl

    success test -e unsolvable.negative
}

for sig in SIGINT SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ
do
    trap "echo received $sig" $sig
done

dir=$(dirname $0)
cd $dir

#  mff doesnt work at all
for planner in fd-cached fd ipc-yahsp-mco ipc-jasper ipc-mercury ff lpg marvin1 marvin2 mpc mp m probe yahsp3
do
    test-non-ff ../$planner-clean
done

# section marvin1-typetest
# 
# fail ../limit.sh -t 1 -- marvin1-clean tidy-p01.pddl tidy-domain.pddl

