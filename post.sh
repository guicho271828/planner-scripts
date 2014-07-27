#! /bin/bash

# moves the output files

vmv output.sas $SAS
for groups in $(ls *.groups 2> /dev/null)
do
    vmv $groups $PROBLEM_NAME.$groups
done
vmv output $SAS_PLUS
vmv elapsed.time $PROBLEM_NAME.time
vmv plan_numbers_and_cost $PROBLEM_NAME.cost

vecho $'\x1b[34;1m'---- process $PPID finished ----------------------------
if [[ -e sas_plan.1 ]]
then
    vecho Result:
    for plan in $(ls sas_plan.*)
    do
        vmv $plan $PROBLEM_NAME.plan.${plan##*.}
        vecho $PROBLEM_NAME.plan.${plan##*.}
    done
    $VERBOSE && cat $PROBLEM_NAME.cost
elif [[ -e sas_plan ]]
then
    vmv sas_plan $PROBLEM_NAME.plan.1
    vecho "Result: $PROBLEM_NAME.plan.1"
    $VERBOSE && cat $PROBLEM_NAME.cost
else
    vecho "Search Failed: No path was found in the current configuration."
fi
vecho --------------------------------------------------------$'\x1b[0m'

# cleanup
vrm -rf $TMPDIR
