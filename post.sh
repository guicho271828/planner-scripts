#! /bin/bash

pkill -9 -P 1 downward
pkill -9 -P 1 downward
pkill -9 -P 1 downward-1
pkill -9 -P 1 downward-1

rm -f $FD_STATUS $TIMEOUT_STATUS
mv output.sas $SAS
for groups in $(ls *.groups 2> /dev/null)
do
    mv $groups $PROBLEM_NAME.$groups
done
mv output $SAS_PLUS
mv elapsed.time $PROBLEM_NAME.time
mv plan_numbers_and_cost $PROBLEM_NAME.cost

echo $'\x1b[34;1m'---- process $PPID finished ----------------------------
if [[ -e sas_plan.1 ]]
then
    echo Result:
    for plan in $(ls sas_plan.*)
    do
        mv $plan $PROBLEM_NAME.plan.${plan##*.}
        echo $PROBLEM_NAME.plan.${plan##*.}
    done
    cat $PROBLEM_NAME.cost
elif [[ -e sas_plan ]]
then
    echo Result:
    mv sas_plan $PROBLEM_NAME.plan.1
    echo $PROBLEM_NAME.plan.1
    cat $PROBLEM_NAME.cost
else
    echo "Search Failed: No path could be found in the current configuration."
fi
echo --------------------------------------------------------$'\x1b[0m'

rm -rfv $TMPDIR
