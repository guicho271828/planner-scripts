#!/bin/bash

echodo (){
    echo $*
    $*
}
export -f echodo

sleep=3
mykill (){
    ps $1 &> /dev/null && {
        pstree -p -H $1 $1
        echodo $SCRDIR/killall.sh $1 -SIGXCPU
        echodo $SCRDIR/killall.sh $1 -SIGTERM
        echodo sleep $sleep
        echo sleep end
        ps $1 &> /dev/null && {
            echodo $SCRDIR/killall.sh $1 -SIGKILL
        }
    }
}
export -f mykill
