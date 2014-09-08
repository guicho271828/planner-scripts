#!/bin/bash

. $SCRDIR/util.sh

################################################################
#### option processing

while getopts ":o:" opt
do
    case ${opt} in
        o)  # specifies the search option
            options=${OPTARG:-$options} ;;
        \?) OPT_ERROR=1; break;;
        * ) echo "unsupported option $opt" ;;
    esac
done

shift $(($OPTIND - 1))

if [[ ( $1 == "" ) || $OPT_ERROR ]]
then
    cat >&2 <<EOF
usage: ./$0
       [-o OPTIONS]        -- search options
       problemfile [domainfile=domain.pddl]
EOF
    exit 2
fi

################################################################
#### argument processing

problem=$(readlink -ef $1)
probname=$(basename $problem .pddl)
probdir=$(dirname $problem)

if [[ $2 != "" ]]
then
    domain=$(readlink -ef $2)
else
    domain=$probdir/domain.pddl
fi
if [[ ! -e $domain ]]
then
    echo "no domain file $domain!" >&2
    exit 1
fi

################################################################
#### common finalization hook (further call finalize)

_finalize (){
    $SCRDIR/killall.sh $pid -15
    cp $STAT $probdir/$probname.stat
    cp log $probdir/$probname.log
    negatively-proven && touch $probdir/$probname.negative
    finalize
}

################################################################
#### run

trap "_finalize" SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ EXIT
pushd $TMP > /dev/null

cp $problem problem.pddl
cp $domain domain.pddl
plan &> log &
pid=$!

if $VERBOSE
then
    tail -f --pid $pid log
else
    wait $pid
fi

