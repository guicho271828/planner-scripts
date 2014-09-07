#!/bin/bash

ccgname=${cgname:=$(whoami)}/$$            # child cgname
cg=/sys/fs/cgroup
cgcpu=$cg/cpuacct/$ccgname
cgmem=$cg/memory/$ccgname
mkdir -p $cgcpu
mkdir -p $cgmem

mem=-1
time=-1

while getopts ":-t:m:" opt
do
    case ${opt} in
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
       [-t time (sec)]
       [-m memory (kB)] command args...
EOF
    exit 2
fi

echo 0 > $cgmem/memory.swappiness

sleep=3
mykill (){
    kill -s SIGXCPU $1
    kill -s SIGTERM $1
    sleep $sleep
    ps $1 &> /dev/null && {
        kill -s SIGKILL $1
    }
}

cgexec -g cpuacct,memory:$ccgname $@ &
pid=$!

cpuusage=$(( $(< $cgcpu/cpuacct.usage) / 1000000 ))
memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))

while ps $pid &> /dev/null
do
    sleep 0.2
    cpuusage=$(( $(< $cgcpu/cpuacct.usage) / 1000000 ))
    memusage=$(( $(< $cgmem/memory.max_usage_in_bytes) / 1024 ))
    if [[ $time -gt 0 && $cpuusage -gt ${time}000 ]]
    then
        echo "cpuacct.usage exceeding. $cpuusage msec." >&2
        mykill $pid
        break
    fi
    if [[ $mem -gt 0 && $memusage -gt $mem ]]
    then
        echo "memory.max_usage_in_bytes exceeding. $memusage kB." >&2
        mykill $pid
        break
    fi
done

wait $pid
exitstatus=$?

case $exitstatus in
    0)
        echo The program successfully finished.
        ;;
    *)
        echo Error occured. status: $exitstatus
        ;;
esac

