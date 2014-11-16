#!/bin/bash

# exported variables:
# SCRDIR -- script dir. the dir of this file
# DIR -- original PWD when this file is invoked
# VERBOSE -- verbosity flag
# DEBUG -- debug flag, disable removing tmp dir when enabled
# STAT -- a path to the stat file. copying this file is user's responsibility
# TMP -- temprary working directory for the planner.

export SCRDIR=$(dirname $(readlink -ef $0))
. $SCRDIR/util.sh
export OPTIONS
export DIR=$PWD
export VERBOSE=false
export DEBUG=false
pcgname=${cgname:-$(whoami)}
cgname=$pcgname/$$            # child cgname
cg=/sys/fs/cgroup
cgcpu=$cg/cpuacct/$cgname
cgmem=$cg/memory/$cgname
mem=-1
time=-1
export ITERATED=false

while getopts ":-vido:t:m:" opt
do
    case ${opt} in
        o)  # specifies the search option
            OPTIONS=${OPTARG:-$OPTIONS} ;;
        v)  # increase verbosity: tail -f search.log during the search
            VERBOSE=true ;; # true
        d)  # do not remove the temporary directory for debugging
            # assume -v
            DEBUG=true ;
            VERBOSE=true ;;
        i)  # use iterated run if possible
            ITERATED=true
        t)  # hard limit of the execution time, in sec.
            time=${OPTARG:-$time} ;
            [[ $time == 'unlimited' ]] && time=-1 ;;
        m)  # limit on the memory usage, in kB.
            mem=${OPTARG:-$mem};
            [[ $mem == 'unlimited' ]] && mem=-1 ;;
        -)  break ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

echo mem:$(($mem/1000))MB, time:${time}sec, cgname:$cgname

shift $(( $OPTIND - 1 ))
if [[ ( $1 == "" ) || $OPT_ERROR ]]
then
    cat >&2 <<EOF
usage: ./limit.sh
       [-v] [-d]
       [-t time (sec)]
       [-m memory (kB)] [--] command args...
 examples:
  limit.sh -v -t 100 -- macroff-clean problem.pddl domain.pddl
EOF
    exit 2
fi

record-stat (){
    until cpuusage=$(( $(< $cgcpu/cpuacct.usage) / 1000000 ))
    do
        sleep 0.2
    done
    until memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
    do
        sleep 0.2
    done
    cat > $TMP/stat <<EOF
cputime $cpuusage
maxmem $memusage
EOF
}

interrupt (){
    mykill $pid
}
finalize (){
    rmdir -v $cgcpu $cgmem
    $DEBUG || rm -rf $TMP
    $DEBUG && echo Debug flag is on, $TMP not removed!
}
trap "interrupt" SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ
trap "finalize" EXIT
mkdir -v $cgcpu
mkdir -v $cgmem
echo 0 > $cgmem/memory.swappiness
echo 1 > $cgmem/memory.use_hierarchy
if [[ $mem -gt 0 ]]
then
    echo $(($mem * 1024)) > $cgmem/memory.limit_in_bytes
    echo $(($mem * 1024)) > $cgmem/memory.memsw.limit_in_bytes
fi

mkdir -p /tmp/newtmp
export TMP=$(mktemp -d --tmpdir=/tmp/newtmp limit.XXXXXXXXXX )

record-stat
export STAT=$(readlink -ef $TMP/stat)

vecho $TMP
command=$(readlink -ef "$SCRDIR/$1") ; shift ;
vecho "current planner options : $OPTIONS"
vechodo cgexec -g cpuacct,memory:$cgname $command $@ &
pid=$!

while ps $pid &> /dev/null
do
    sleep 0.2
    record-stat
    if [[ $time -gt 0 && $cpuusage -gt ${time}000 ]]
    then
        echo "cpuacct.usage exceeding. $cpuusage msec." >&2
        mykill $pid
    fi
    if [[ $mem -gt 0 && $memusage -gt $mem ]]
    then
        echo "memory.max_usage_in_bytes exceeding. $memusage kB." >&2
        mykill $pid
    fi
done

wait $pid
exitstatus=$?
case $exitstatus in
    0) echo The program successfully finished. ;;
    *) echo Error occured. status: $exitstatus ;;
esac

