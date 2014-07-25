#! /bin/bash

FD_DIR=~/repos/downward
REALPATH=$(which realpath)
if [[ $REALPATH == "" ]]
then
    REALPATH="readlink -e "
fi

################################################################
#### default options

export TIMER='eval /usr/bin/time -f "real %e\nuser %U\nsys %S\nmaxmem %M"'
export SOFT_TIME_LIMIT=1795
export HARD_TIME_LIMIT=1800
export MEMORY_USAGE=2000000
export OPTIONS="ipc seq-sat-lama-2011"
export VERBOSE=false

################################################################
#### option processing

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

if [[ ( $1 == "" ) || $OPT_ERROR ]]
then
    cat >&2 <<EOF
usage: ./test-problem
       [-v]
       [-t SOFT_TIME_LIMIT]
       [-T HARD_TIME_LIMIT]
       [-m MEMORY_LIMIT]
       [-o FD_OPTIONS] problemfile [domainfile]
EOF
    exit 2
fi

re='^[0-9]+$'
if [[ ( $SOFT_TIME_LIMIT =~ $re ) && ( $HARD_TIME_LIMIT =~ $re ) ]]
then
    if [ $SOFT_TIME_LIMIT -gt $HARD_TIME_LIMIT ]
    then
        echo "ERROR: the soft time limit should be less than equal to the hard limit" >&2
        exit 1
    fi
fi

################################################################
# more setup codes

export SCR_DIR=$(dirname $($REALPATH $0))
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
export TRANSLATE=$FD_DIR/src/translate/translate.py
export PREPROCESS=$FD_DIR/src/preprocess/preprocess # < OUTPUT.SAS
export SEARCH_DIR=$FD_DIR/src/search
export SEARCH="$SEARCH_DIR/downward $OPTIONS"

export FD_PID
export TIMEOUT_PID
export finished

################################################################
#### functions

finalize (){
    echo "Killing FD_PID=$FD_PID subprocess..."
    $SCR_DIR/killall.sh $FD_PID -9
    echo "Killing TIMEOUT_PID=$TIMEOUT_PID subprocess..."
    $SCR_DIR/killall.sh $TIMEOUT_PID -9
    echo "Killing TAIL_PID=$TAIL_PID subprocess..."
    $SCR_DIR/killall.sh $TAIL_PID -9
    $SCR_DIR/post.sh
}

fd (){
    ulimit -v $MEMORY_USAGE -t $HARD_TIME_LIMIT
    $TIMER $TRANSLATE $DOMAIN $PDDL &> $PROBLEM_NAME.translate.log || hard_limit
    echo Translation Finished
    $TIMER $PREPROCESS < output.sas &> $PROBLEM_NAME.preprocess.log  || hard_limit
    echo Preprocessing Finished
    $TIMER $SEARCH < output &> $PROBLEM_NAME.search.log || hard_limit
    echo Search Finished
    echo 0 > $finished
}

hard_limit (){
    echo "Reached the Hard limit, terminating"
    echo 1 > $finished
}
export -f hard_limit

soft_limit (){
    if [[ $SOFT_TIME_LIMIT =~ ^[0-9]+$ ]]
    then
        sleep $SOFT_TIME_LIMIT
        if ls sas_plan* &> /dev/null
        then # パスが一つでもあれば終了
            echo "PID ($$): Reached the SOFT limit. Path found, $FD_PID terminated"
        else # なければ hard limit に至るまで続行
            echo "PID ($$): Reached the SOFT limit. Continue searching..." >&2
            # touch sas_plan sas_plan.1 # for optimising / satisficing track
            inotifywait \
                --exclude ".*\.sas" \
                --exclude ".*\.groups" \
                --exclude ".*\.time" \
                --exclude "output" \
                --exclude "plan_numbers_and_cost" \
                -e create ./
            echo "PID ($$): Path found, $FD_PID terminated"
        fi
        echo 0 > $finished
    else
        echo "ERROR: the soft time limit is malformed -- $SOFT_TIME_LIMIT" >&2
        echo 1 > $finished
    fi
}

timeout (){
    case $SOFT_TIME_LIMIT in
        unlimited) ;;
        soft) ;;
        hard) ;;
        *) soft_limit ;;
    esac
}

################################################################
#### main code

# cleanup
rm -f $PROBLEM_NAME.time
rm -f $PROBLEM_NAME.cost
rm -f $PROBLEM_NAME.*.log
rm -f $PROBLEM_NAME.plan*
rm -f $SAS $SAS_PLUS

echo $'\x1b[34;1m'---- process $PPID started -----------------------------
echo "MAX MEM(kB):          $MEMORY_USAGE"
echo "SOFT TIME LIMIT(sec): $SOFT_TIME_LIMIT"
echo "HARD TIME LIMIT(sec): $HARD_TIME_LIMIT"
echo "PROBLEM_NAME:         $PROBLEM_NAME"
echo "DOMAIN:               $DOMAIN"
echo "SEARCH COMMAND:       $SEARCH"
echo --------------------------------------------------------$'\x1b[0m'

export TMPDIR=$(mktemp -d)
pushd $TMPDIR
export finished=$(mktemp)
if $VERBOSE
then
    touch $PROBLEM_NAME.search.log
    tail -f $PROBLEM_NAME.search.log &
    TAIL_PID=$!
fi

fd &
export FD_PID=$!
timeout &
export TIMEOUT_PID=$!
echo "FD      Process $FD_PID"
echo "TIMEOUT Process $TIMEOUT_PID"
trap "finalize" EXIT

inotifywait $finished
exit $(cat $finished)
