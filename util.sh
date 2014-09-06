#!/bin/bash

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
