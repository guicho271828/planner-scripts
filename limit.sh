#!/bin/bash

echo "Running limit.sh($$): $@"

# ********  Exported variables: **********
# 
# SCRDIR -- script dir. the dir of this file
# DIR -- original PWD when this file is invoked
# DEBUG -- debug flag, disable removing tmp dir when enabled
# TMP -- temprary working directory for the planner.

export SCRDIR=$(dirname $(readlink -ef $0))
. $SCRDIR/util.sh
export OPTIONS=
export DIR=$PWD
export DEBUG=false

mem=
time=
export ITERATED=false

while getopts ":-vido:t:m:" opt; do {
        case ${opt} in
            o)  # specifies the search option
                OPTIONS=${OPTARG:-$OPTIONS} ;;
            d)  # do not remove the temporary directory for debugging
                DEBUG=true ;;
            i)  # use iterated run if possible
                ITERATED=true ;;
            t)  # hard limit of the execution time, in sec.
                time=${OPTARG} ;
                [[ $time == -1 ]] && time= ;
                [[ $time == 'unlimited' ]] && time= ;;
            m)  # limit on the memory usage, in kB.
                mem=${OPTARG};
                [[ $mem == -1 ]] && mem= ;
                [[ $mem == 'unlimited' ]] && mem= ;;
            -)  break ;;
            \?) OPT_ERROR=1; break;;
            * ) echo "limit.sh($$): unsupported option $opt" ;;
        esac
    }
done

shift $(( $OPTIND - 1 ))
if [[ ( $1 == "" ) || $OPT_ERROR ]] ; then {
        cat >&2 <<EOF
usage: ./limit.sh
       [-d]
       [-t time (sec)]
       [-m memory (kB)] [--] command args...
 examples:
  limit.sh -t 100 -- macroff-clean problem.pddl domain.pddl
EOF
        exit 2
    }
fi

echo "limit.sh($$): mem: ${mem:-unlimited} kB, time: ${time:-unlimited} sec"
echo "limit.sh($$): running at $TMP"
echo "limit.sh($$): command to execute: $command"
echo "limit.sh($$): current planner options : $OPTIONS"
echo "limit.sh($$): note: time precision is 0.5 sec"

echodo TIMEOUT_IDSTR="LIMIT_SH " $SCRDIR/timeout/timeout -x 0.1 -c \
    $([ -z $mem  ] || echo "--memlimit-rss $mem") \
    $([ -z $time ] || echo "-t            $time") \
    $@

exitstatus=$?
case $exitstatus in
    0) echo "limit.sh($$): The program successfully finished." ;;
    *) echo "limit.sh($$): Error occured. status: $exitstatus" ;;
esac

exit $exitstatus
