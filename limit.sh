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
export DIR=$PWD
export VERBOSE=false
export DEBUG=false
ccgname=${cgname:=$(whoami)}/$$            # child cgname
cg=/sys/fs/cgroup
cgcpu=$cg/cpuacct/$ccgname
cgmem=$cg/memory/$ccgname
mem=-1
time=-1

while getopts ":-vdt:m:" opt
do
    case ${opt} in
        v)  # increase verbosity: tail -f search.log during the search
            VERBOSE=true ;; # true
        d)  # do not remove the temporary directory for debugging
            # assume -v
            DEBUG=true ;
            VERBOSE=true ;;
        t)  # hard limit of the execution time, in sec.
            time=${OPTARG};;
        m)  # limit on the memory usage, in kB.
            mem=${OPTARG};;
        -)  break ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

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
    cpuusage=$(( $(< $cgcpu/cpuacct.usage) / 1000000 ))
    memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
    cat > $TMP/stat <<EOF
cputime $cpuusage
maxmem $memusage
EOF
}

finalize (){
    wait
    rmdir $cgcpu $cgmem
    $DEBUG || rm -rf $TMP
    $DEBUG && echo Debug flag is on, $TMP not removed!
}
trap "finalize" SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ EXIT
vechodo mkdir -p $cgcpu
vechodo mkdir -p $cgmem
echo 0 > $cgmem/memory.swappiness

vechodo mkdir -p /tmp/newtmp
export TMP=$(mktemp -d --tmpdir=/tmp/newtmp limit.XXXXXXXXXX )

record-stat
export STAT=$(readlink -ef $TMP/stat)

vecho $TMP
command=$(readlink -ef "$SCRDIR/$1") ; shift ;
vechodo cgexec -g cpuacct,memory:$ccgname $command $@ &
pid=$!

while ps $pid &> /dev/null
do
    sleep 0.2
    record-stat
    if [[ $time -gt 0 && $cpuusage -gt ${time}000 ]]
    then
        echo "cpuacct.usage exceeding. $cpuusage msec." >&2
        mykill $pid ; break
    fi
    if [[ $mem -gt 0 && $memusage -gt $mem ]]
    then
        echo "memory.max_usage_in_bytes exceeding. $memusage kB." >&2
        mykill $pid ; break
    fi
done

wait $pid
exitstatus=$?
case $exitstatus in
    0) echo The program successfully finished. ;;
    *) echo Error occured. status: $exitstatus ;;
esac

