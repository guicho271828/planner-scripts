#!/bin/bash

vecho (){
    $VERBOSE && echo $@
}
export -f vecho

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
export -f vrm
export -f vmv
export -f vcp
