#! /bin/bash

# @(#) usage: [-v] [-t SOFT_TIME_LIMIT] [-T HARD_TIME_LIMIT] [-m MEMORY_LIMIT] [-o FD_OPTIONS] problemfile [domainfile]


REALPATH=$(which realpath)
if [[ $REALPATH == "" ]]
then
    REALPATH="readlink -e "
fi

export SOFT_TIME_LIMIT=1795
export HARD_TIME_LIMIT=1800
export MEMORY_USAGE=2000000
export OPTIONS="ipc seq-sat-lama-2011"
VERBOSE=false

while getopts ":vst:T:m:o:" opt
do
    case ${opt} in
        v)  # increase verbosity: tail -f search.log during the search
            VERBOSE=true ;; # true
        t)  # soft limit of the downward execution time
            # if no path was found when the time reached the limit
            # continues to search until the hard limit is reached.
            # if the path was already found then finishes
            # default value is 0 sec, which means it uses the hard limit by default
            SOFT_TIME_LIMIT=${OPTARG:-$SOFT_TIME_LIMIT} ;;
        T)  # hard limit of the downward execution time
            # default value is 30 min (same as ICAPS)
            HARD_TIME_LIMIT=${OPTARG:-$HARD_TIME_LIMIT} ;;
        m)  # limit memory usage under 1 GB
            MEMORY_USAGE=${OPTARG:-$MEMORY_USAGE} ;;
        o)  # specifies the search option
            OPTIONS=${OPTARG:-$OPTIONS} ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

shift $(($OPTIND - 1))

if [ $OPT_ERROR ]; then      # option error
    echo "usage: [-v] [-t SOFT_TIME_LIMIT] [-T HARD_TIME_LIMIT] [-m MEMORY_LIMIT] [-o FD_OPTIONS] problemfile [domainfile]" >&2
    exit 1
fi

if [ $SOFT_TIME_LIMIT -gt $HARD_TIME_LIMIT ]
then
    echo "the soft time limit should be less than equal to the hard limit" >&2
    exit 1
fi

export PDDL=$($REALPATH $1)

if [[ $2 != "" ]]
then
    export DOMAIN=$($REALPATH $2)
else
    export DOMAIN=${PDDL%/*}/domain.pddl
fi

if [[ $PDDL =~ .*\.pddl ]]
then
    export PROBLEM_NAME=${PDDL%.pddl}
elif [[ $PDDL =~ .*pfile.* ]]
then
    # pfile1 etc...
    export PROBLEM_NAME=$PDDL
fi  

export SAS=$PROBLEM_NAME.sas
export SAS_PLUS=$PROBLEM_NAME.sasp
FD_DIR=~/repos/downward

rm -f $PROBLEM_NAME.time
rm -f $PROBLEM_NAME.cost
rm -f $PROBLEM_NAME.*.log
rm -f $PROBLEM_NAME.plan*
rm -f $SAS
rm -f $SAS_PLUS

export TRANSLATE=$FD_DIR/src/translate/translate.py
export PREPROCESS=$FD_DIR/src/preprocess/preprocess # < OUTPUT.SAS
SEARCH_DIR=$FD_DIR/src/search
export SEARCH="$SEARCH_DIR/downward $OPTIONS"

echo $'\x1b[34;1m'---- process $PPID started -----------------------------
echo "MAX MEM(kB):          $MEMORY_USAGE"
echo "SOFT TIME LIMIT(sec): $SOFT_TIME_LIMIT"
echo "HARD TIME LIMIT(sec): $HARD_TIME_LIMIT"
echo "PROBLEM_NAME:         $PROBLEM_NAME"
echo "DOMAIN:               $DOMAIN"
echo "SEARCH COMMAND:       $SEARCH"
echo --------------------------------------------------------$'\x1b[0m'

TMPDIR=`mktemp -d`
trap "rm -rfv $TMPDIR" SIGINT
trap "rm -rfv $TMPDIR" EXIT
pushd $TMPDIR

touch $PROBLEM_NAME.translate.log
touch $PROBLEM_NAME.preprocess.log
touch $PROBLEM_NAME.search.log
if $VERBOSE
then
    # tail -f $PROBLEM_NAME.translate.log &
    # tail -f $PROBLEM_NAME.preprocess.log &
    tail -f $PROBLEM_NAME.search.log &
fi

export FD_STATUS=$(mktemp)
export TIMEOUT_STATUS=$(mktemp)

descendants () {
    parents[0]=$1
    while [ ${#parents[@]} -gt 0 ]
    do
        echo ${parents[0]}
        children=($(pgrep -P ${parents[0]}))
        unset parents[0]
        parents=("${parents[@]}" "${children[@]}")
    done
}

killDescendants (){
    echo $*
    for PID in $*
    do
        echo killing : $(ps -e | grep $PID)
        kill $PID 
    done
}


coproc FD {
    ulimit -v $MEMORY_USAGE -t $HARD_TIME_LIMIT
    
    $TRANSLATE $DOMAIN $PDDL &> $PROBLEM_NAME.translate.log
    echo Translation Finished

    $PREPROCESS < output.sas &> $PROBLEM_NAME.preprocess.log
    echo Preprocessing Finished

    $SEARCH < output &> $PROBLEM_NAME.search.log

    echo $? > $FD_STATUS
}

coproc TIMEOUT {
    sleep $SOFT_TIME_LIMIT
    echo t > $TIMEOUT_STATUS
}

echo "Script  Process $$"
echo "FD      Process $FD_PID"
echo "TIMEOUT Process $TIMEOUT_PID"

CHECK_INTERVAL=5
if [ $SOFT_TIME_LIMIT -lt $CHECK_INTERVAL ]
then
    CHECK_INTERVAL=$SOFT_TIME_LIMIT
fi

FD_DESCENDANTS=$(descendants $FD_PID)
TIMEOUT_DESCENDANTS=$(descendants $TIMEOUT_PID)
echo $FD_DESCENDANTS
echo $TIMEOUT_DESCENDANTS
while true
do
    sleep $CHECK_INTERVAL
    if [[ $(cat $FD_STATUS) != "" ]] # 何か書き込まれている = FDが終了
    then
        if [[ $(cat $FD_STATUS) == 0 ]] # 正常終了
        then
            echo "PID ($$): Search finished normally." >&2
        else # ulimit による強制終了
            echo "PID ($$): Reached the HARD limit, $FD_PID terminated" >&2 
        fi
        break
    elif [[ $(cat $TIMEOUT_STATUS) == t ]] # soft timeout
    then
        if ls sas_plan* &> /dev/null
        then # パスが一つでもあれば終了
            echo "PID ($$): Reached the SOFT limit. Path found, $FD_PID terminated" >&2
            break
        else # なければ hard limit に至るまで続行
            echo "PID ($$): Reached the SOFT limit. Continue searching..." >&2
        fi
    fi
done

killDescendants $FD_DESCENDANTS
killDescendants $TIMEOUT_DESCENDANTS
pkill -9 -P 1 downward
pkill -9 -P 1 downward

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

popd

exit