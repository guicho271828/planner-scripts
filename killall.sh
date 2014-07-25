#!/bin/bash

if [[ $2 == "" ]]
then
    cat <<EOF
usage: killall.sh PID KILLOPTIONS
EOF
    exit 1
fi

target=$1
shift 1
children=$(pgrep -P $target)

# cat <<EOF
# PID:              $target
# KILLOPTIONS:      $@
# echo children:    $children
# EOF

pstree -pl $target
for pid in $children
do
    if [[ $pid != $$ ]]
    then
        $0 $pid $@
    fi
done

echo "kill $@ $target"
kill $@ $target
