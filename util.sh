
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


