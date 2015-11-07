#!/bin/bash

tmpdir=${tmpdir:-$(mktemp -d)}

echo $tmpdir

success (){
    echo $'\x1b[32;1m'"TEST: $@"$'\x1b[0m'
    if $@
    then
        echo -n "$results"$'\x1b[32;1m'✔$'\x1b[0m' >> $tmpdir/results
    else
        echo -n "$results"$'\x1b[31;1m'✘$'\x1b[0m' >> $tmpdir/results
        echo "Unexpected Failure: $@" >> $tmpdir/report
    fi
}

fail (){
    echo $'\x1b[32;1m'"TEST: $@"$'\x1b[0m'
    if $@
    then
        echo -n "$results"$'\x1b[31;1m'✘$'\x1b[0m' >> $tmpdir/results
        echo "Unexpected Success: $@" >> $tmpdir/report
    else
        echo -n "$results"$'\x1b[32;1m'✔$'\x1b[0m' >> $tmpdir/results
    fi
}

report (){
    cat $tmpdir/results
    echo Details:
    cat $tmpdir/report
}

trap report EXIT
