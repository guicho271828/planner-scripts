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
export MEMORY_USAGE=2000000 # kB
export OPTIONS="ipc seq-sat-lama-2011"
export VERBOSE=false

################################################################
#### option processing

while getopts ":vt:T:m:o:" opt
do
    case ${opt} in
        v)  # increase verbosity: tail -f search.log during the search
            VERBOSE=true ;; # true
        t)  # soft limit of the downward execution time, in seconds.
            # if no path was found when the time reached the limit
            # continues to search until the hard limit is reached.
            # if the path was already found then finishes
            # default value is 0 sec, which means it uses the hard limit by default
            SOFT_TIME_LIMIT=${OPTARG:-$SOFT_TIME_LIMIT} ;;
        T)  # hard limit of the downward execution time
            # default value is 30 min (same as ICAPS)
            HARD_TIME_LIMIT=${OPTARG:-$HARD_TIME_LIMIT} ;;
        m)  # limit on the memory usage, in kB.
            MEMORY_USAGE=${OPTARG:-$MEMORY_USAGE} ;;
        o)  # specifies the search option
            OPTIONS=${OPTARG:-$OPTIONS} ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

vecho "$0 $@"

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
        echo "WARN: the soft time limit should be less than equal to the hard limit" >&2
        echo "WARN: soft: $SOFT_TIME_LIMIT hard: $HARD_TIME_LIMIT" >&2
        echo "WARN: Resetting soft limi to \$HARD_TIME_LIMIT - 1: $(($HARD_TIME_LIMIT - 1))" >&2
        SOFT_TIME_LIMIT=$(($HARD_TIME_LIMIT - 1))
    fi
fi

################################################################
# more setup codes

export SCR_DIR=$(dirname $($REALPATH $0))
export PROBLEM=$($REALPATH $1)

if [[ $2 != "" ]]
then
    export DOMAIN=$($REALPATH $2)
else
    export DOMAIN=${PROBLEM%/*}/domain.pddl
fi

if [[ $PROBLEM =~ .*\.pddl ]]
then
    export PROBLEM_NAME=${PROBLEM%.pddl}
elif [[ $PROBLEM =~ .*pfile.* ]]
then
    # pfile1 etc...
    export PROBLEM_NAME=$PROBLEM
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

vecho (){
    $VERBOSE && echo $@
}
export -f vecho
vmv (){
    if $VERBOSE
    then
        mv -v $@
    else
        mv $@
    fi
}
export -f vmv
vrm (){
    if $VERBOSE
    then
        rm -v $@
    else
        rm $@
    fi
}
export -f vrm

finalize (){
    echo "exit status : $(cat $finished)"
    vecho "Killing FD_PID=$FD_PID subprocess..."
    $SCR_DIR/killall.sh $FD_PID -9
    vecho "Killing TIMEOUT_PID=$TIMEOUT_PID subprocess..."
    $SCR_DIR/killall.sh $TIMEOUT_PID -9
    vecho "Killing TAIL_PID=$TAIL_PID subprocess..."
    $SCR_DIR/killall.sh $TAIL_PID -9
    vecho "Killing INOTIFY_PID=$INOTIFY_PID subprocess..."
    $SCR_DIR/killall.sh $INOTIFY_PID -9
    $SCR_DIR/post.sh
}

fd (){
    ulimit -v $MEMORY_USAGE -t $HARD_TIME_LIMIT
    $TIMER $TRANSLATE $DOMAIN $PROBLEM &> $PROBLEM_NAME.translate.log \
        || ( hard_limit translation && return 1 )
    vecho Translation Finished
    $TIMER $PREPROCESS < output.sas &> $PROBLEM_NAME.preprocess.log \
        || ( hard_limit preprocess && return 1 )
    vecho Preprocessing Finished
    $TIMER $SEARCH < output &> $PROBLEM_NAME.search.log \
        || ( hard_limit search && return 1 )
    vecho Search Finished
    echo 0 > $finished
}

hard_limit (){
    vecho "WARN: the program was terminated during $1"
    echo 1 > $finished
}
export -f hard_limit

soft_limit (){
    if [[ $SOFT_TIME_LIMIT =~ ^[0-9]+$ ]]
    then
        sleep $SOFT_TIME_LIMIT
        if ls sas_plan* &> /dev/null
        then # パスが一つでもあれば終了
            vecho "PID ($$): Reached the SOFT limit. Path found, $FD_PID terminated"
        else # なければ hard limit に至るまで続行
            vecho "PID ($$): Reached the SOFT limit. Continue searching..." >&2
            if [[ ! ( -e sas_plan || -e sas_plan.1 ) ]]
            then
                touch sas_plan sas_plan.1
                inotifywait $(if ! $VERBOSE ; then echo -qq ; fi) sas_plan sas_plan.1
            fi
            vecho "PID ($$): Path found, $FD_PID terminated"
        fi
        echo 0 > $finished
    else
        vecho "ERROR: the soft time limit is malformed -- $SOFT_TIME_LIMIT" >&2
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
vrm -f $PROBLEM_NAME.time
vrm -f $PROBLEM_NAME.cost
vrm -f $PROBLEM_NAME.*.log
vrm -f $PROBLEM_NAME.plan*
vrm -f $SAS $SAS_PLUS

vecho $'\x1b[34;1m'---- process $PPID started -----------------------------
vecho "MAX MEM(kB):          $MEMORY_USAGE"
vecho "SOFT TIME LIMIT(sec): $SOFT_TIME_LIMIT"
vecho "HARD TIME LIMIT(sec): $HARD_TIME_LIMIT"
vecho "PROBLEM:              $PROBLEM"
vecho "DOMAIN:               $DOMAIN"
vecho "SEARCH COMMAND:       $SEARCH"
vecho --------------------------------------------------------$'\x1b[0m'

export TMPDIR=$(mktemp -d --tmpdir fastdownward.XXXXXX)
if $VERBOSE
then
    pushd $TMPDIR
else
    pushd $TMPDIR > /dev/null
fi
export finished=$(mktemp finished.XXXXXX)

if $VERBOSE
then
    touch $PROBLEM_NAME.search.log
    tail -f $PROBLEM_NAME.search.log &
    TAIL_PID=$!
fi

fd 2> /dev/null &
export FD_PID=$!
timeout 2> /dev/null &
export TIMEOUT_PID=$!

vecho "FD      Process $FD_PID"
vecho "TIMEOUT Process $TIMEOUT_PID"

trap "finalize" SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ EXIT

inotifywait $(if ! $VERBOSE ; then echo -qq ; fi) -e modify $finished &
INOTIFY_PID=$!
if [[ $(cat $finished) == "" ]] # fd might already finished
then
    wait $INOTIFY_PID
fi
exit $(cat $finished)
