
sleep=3
mykill (){
    ps $1 &> /dev/null && {
        pstree -p -H $1 $1
        $SCRDIR/killall.sh $1 -SIGXCPU
        $SCRDIR/killall.sh $1 -SIGTERM
        sleep $sleep
        echo sleep end
        ps $1 &> /dev/null && {
            $SCRDIR/killall.sh $1 -SIGKILL
        }
    }
}
export -f mykill


fdscript=

find-fd-py (){
    find $1 -name "fast-downward.py"
}

find-fd-upward (){
    echo "finding FD installation from $1 ..."
    if [[ -d $1/downward ]]
    then
        fdscript=$(readlink -ef $(find-fd-py $1/downward))
    else
        if [[ $1 == / ]]
        then
            echo "failed to find Fast Downward installation (fast-downward.py) !"
            exit 1
        else
            find-fd-upward $(readlink -ef $1/..)
        fi
    fi
}

find-fd-upward $(dirname $(readlink -ef $0))
