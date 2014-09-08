#!/bin/bash

vechodo (){
    vecho $*
    $*
}

vecho (){
    $VERBOSE && echo $@
}

v (){
    name=$1
    shift
    if $VERBOSE
    then
        $name -v $@
    else
        $name $@
    fi
}

vmv (){
    v mv $@
}
vrm (){
    v rm $@
}
vcp (){
    v cp $@
}

export -f vecho vrm vmv vcp v

sleep=3
mykill (){
    ps $1 &> /dev/null && {
        kill -s SIGXCPU $1
        kill -s SIGTERM $1
        sleep $sleep
        ps $1 &> /dev/null && {
            kill -s SIGKILL $1
        }
    }
}
export -f mykill
