
. $SCRDIR/util.sh

################################################################
#### option processing

shift $(($OPTIND - 1))

if [[ ( $1 == "" ) || $OPT_ERROR ]]
then
    cat >&2 <<EOF
Usage: ./$0
       [-o OPTIONS]        -- search options
       problemfile [domainfile=domain.pddl]
EOF
    exit 2
fi

################################################################
#### argument processing

problem=$1
probname=$(basename $problem .pddl)
probdir=$(readlink -ef $(dirname $problem))
problem=$probdir/$probname.pddl

if [[ $2 != "" ]]
then
    domain=$2
    domain=$(readlink -ef $(dirname $domain))/$(basename $domain)
else
    domain=$probdir/domain.pddl
fi
if [[ ! -e $domain ]]
then
    domain=$probdir/$probname-domain.pddl
fi
if [[ ! -e $domain ]]
then
    echo "no domain file $domain!" >&2
    exit 1
fi

################################################################
#### output files

log=$probdir/$probname.log
err=$probdir/$probname.err
neg=$probdir/$probname.negative

################################################################
#### temporary directory
mkdir -p /tmp/$(whoami)
tmp=$(mktemp -d --tmpdir=/tmp/$(whoami))

################################################################
#### common finalization hook (further call finalize)
# automatically copies the log and stat file
# but not plan files: because they are planner specific

_interrupt (){
    echo "common.sh($$): received $1, exiting..."
    exit 1
}
_finalize (){
    echo "common.sh($$): forcibly killing all subprocesses"
    $SCRDIR/killall.sh $pid -9
    negatively-proven && touch $neg
    finalize                    # call planner-specific finalizer
    echo $'\x1b[32;1m'--------------------------------------------------------
    echo Result:
    report-results 2> /dev/null
    echo --------------------------------------------------------$'\x1b[0m'
    plan-found
    status=$?
    
    $DEBUG || rm -rf $tmp
    $DEBUG && echo "common.sh($$): Debug flag is on, $tmp not removed!"
    
    exit $status
}

for sig in SIGINT SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ
do
    trap "_interrupt $sig" $sig
done
trap "_finalize" EXIT

################################################################
#### run

cd $tmp

ln -s $problem problem.pddl
ln -s $domain domain.pddl
plan >(tee $log) 2>(tee $err >&2)
