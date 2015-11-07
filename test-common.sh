#!/bin/bash

results=${results:-}

success (){
    echo $'\x1b[32;1m'"TEST: $@"$'\x1b[0m'
    if $@
    then
        results="$results"$'\x1b[32;1m'✔$'\x1b[0m'
    else
        results="$results"$'\x1b[31;1m'✘$'\x1b[0m'
    fi
}

fail (){
    echo $'\x1b[32;1m'"TEST: $@"$'\x1b[0m'
    if $@
    then
        results="$results"$'\x1b[31;1m'✘$'\x1b[0m'
    else
        results="$results"$'\x1b[32;1m'✔$'\x1b[0m'
    fi
}

report (){
    echo $results
}

trap report EXIT
