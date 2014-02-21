#! /bin/bash

PARALLEL=1

sort=
SCRIPT_NAME=test-problem.sh
RUN=true
ALLOW_RANDOM=false
FROM=0
LIMIT=1024
while getopts ":S:p:s:f:drl:" opt
do
    case ${opt} in
        p) PARALLEL=${OPTARG} ;;
        s) SCRIPT_NAME=${OPTARG} ;;
        S) sort=${OPTARG} ;;
        d) RUN=false;;
        r) ALLOW_RANDOM=true;;
        l) LIMIT=${OPTARG} ;;
        f) FROM=${OPTARG} ;;
        *) echo "unsupported option $opt" ;;
    esac
done

shift $(($OPTIND - 1))

if [[ $1 == "" ]]
then
    OPT_ERROR=1
fi

if [ $OPT_ERROR ]; then      # option error
    cat <<EOF
usage: [-p NUMPROCS] [-s sctipt] [-l N] [-d] directory pattern [options...]
  -d       -- dry-run
  -p       -- max number of processes
  -l       -- max number of problems to search
  -s       -- script name, defaulted to test-problem.sh (fast downward)
  pattern  -- a pattern for pathname, matches to the tail of each file
  options  -- options to the script
EOF
    exit 1
fi


SDIR=$(readlink -e ${0%/*})
RAND_SRC=$SDIR/random
SCRIPT=$(readlink -e $SDIR/$SCRIPT_NAME)
DIR=$(readlink -e $1)

# echo $0
# echo ${0%%/*}
# echo $SDIR
# echo $SCRIPT
echo $DIR

# $1 : フォルダ名
# $2 : the prefix of each problem

PATTERN=$2

shift 2

echo Search options : $*
allfiles=$(find $DIR -regex "$PATTERN")

if $ALLOW_RANDOM
then
    files=$(ls -1 $sort $allfiles | shuf --random-source=$RAND_SRC | tail -n +$(expr $FROM + 1) - | head -n $LIMIT -)
else
    files=$(ls -1 $sort $allfiles | tail -n +$(expr $FROM + 1) - | head -n $LIMIT -)
fi
cat <<EOF
Files to search:
  $(echo $allfiles | wc -w) matched in total,
  first $FROM ignored,
  limited up tp $LIMIT files,
  actually $(echo $files | wc -w) files to search.
  the next index is $(expr $FROM + $(echo $files | wc -w))
EOF

pairs=

for file in $files
do
    pairs="$pairs
$file"
    pairs="$pairs
${file%/*}/domain.pddl"
done

echo "(echo $pairs) | xargs -n 2 -P $PARALLEL $SCRIPT $@"

if $RUN
then
    pushd $DIR
    (echo $pairs) | xargs -n 2 -P $PARALLEL $SCRIPT "$@"
    popd
fi
