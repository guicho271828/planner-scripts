#!/bin/bash

echo "Running limit.sh($$): $@"
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

cg=/sys/fs/cgroup
cpuacct=$(awk -F: '/cpuacct/{print $3}' /proc/self/cgroup)/$$
memory=$(awk -F: '/memory/{print $3}' /proc/self/cgroup)/$$
cgcpu=$cg/cpuacct$cpuacct
cgmem=$cg/memory$memory

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
            ITERATED=true ;;
        t)  # hard limit of the execution time, in sec.
            time=${OPTARG:-$time} ;
            [[ $time == 'unlimited' ]] && time=-1 ;;
        m)  # limit on the memory usage, in kB.
            mem=${OPTARG:-$mem};
            [[ $mem == 'unlimited' ]] && mem=-1 ;;
        -)  break ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "limit.sh($$): unsupported option $opt" ;;
    esac
done

$VERBOSE && echo "limit.sh($$): mem:$(($mem/1000))MB, time:${time}sec"

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
    walltime=$(($(date +%s)-$start))
    until cpuusage=$(( $(< $cgcpu/cpuacct.usage) / 1000000 ))
    do
        sleep 0.2
    done
    until memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
    do
        sleep 0.2
    done
    cat > $TMP/stat <<EOF
walltime $walltime
cputime $cpuusage
maxmem $memusage
EOF
}

interrupt (){
    target=$(pgrep -P $pid)
    echo "limit.sh($$): received $1, killing subprocess $target with $1"
    vechodo kill -s $1 $target
    while ps -p $target &>/dev/null
    do
        sleep 0.5
        pstree -pl $pid
    done
    echo "limit.sh($$): killed subprocess $target died"
}
finalize (){
    rmdir $($VERBOSE && echo -v) $cgcpu $cgmem
    $DEBUG || rm $($VERBOSE && echo -v) -rf $TMP
    $DEBUG && echo "limit.sh($$): Debug flag is on, $TMP not removed!"
}
for sig in SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ
do
    trap "interrupt $sig" $sig
done 
trap "finalize" EXIT
echo "limit.sh($$): removing and making cgdir"
while [[ -e $cgcpu || -e $cgmem ]]
do
    rmdir $($VERBOSE && echo -v) $cgcpu $cgmem
done
mkdir $($VERBOSE && echo -v) -p $cgcpu
mkdir $($VERBOSE && echo -v) -p $cgmem
echo 0 > $cgmem/memory.swappiness
echo 1 > $cgmem/memory.use_hierarchy
if [[ $mem -gt 0 ]]
then
    echo $(($mem * 1024)) > $cgmem/memory.limit_in_bytes
    # some kernels do not have memsw
    bash -c "echo $(($mem * 1024)) > $cgmem/memory.memsw.limit_in_bytes" 2>/dev/null
fi

mkdir $($VERBOSE && echo -v) -p /tmp/newtmp
export TMP=$(mktemp -d --tmpdir=/tmp/newtmp limit.XXXXXXXXXX )

start=$(date +%s)
record-stat
export STAT=$(readlink -ef $TMP/stat)

vecho "limit.sh($$): $TMP"
command=$(readlink -ef "$SCRDIR/$1") ; shift ;
vecho "limit.sh($$): current planner options : $OPTIONS"
vechodo cgexec -g cpuacct:$cpuacct -g memory:$memory $command $@ &
pid=$!

while ps $pid &> /dev/null
do
    sleep 0.2
    record-stat
    # if [[ $time -gt 0 && $walltime -gt $time ]]
    # then
    #     echo "limit.sh($$): walltime exceeding. $walltime sec." >&2
    #     mykill $pid
    # fi
    if [[ $time -gt 0 && $cpuusage -gt ${time}000 ]]
    then
        echo "limit.sh($$): cpuacct.usage exceeding. $cpuusage msec." >&2
        mykill $pid
    fi
    if [[ $mem -gt 0 && $memusage -gt $mem ]]
    then
        echo "limit.sh($$): memory.max_usage_in_bytes exceeding. $memusage kB." >&2
        mykill $pid
    fi
done

wait $pid
exitstatus=$?
case $exitstatus in
    0) $VERBOSE && echo "limit.sh($$): The program successfully finished." ;;
    *) echo "limit.sh($$): Error occured. status: $exitstatus" ;;
esac

exit $exitstatus
