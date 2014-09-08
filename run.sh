#!/bin/bash

trap "finalize" SIGHUP SIGQUIT SIGABRT SIGSEGV SIGTERM SIGXCPU SIGXFSZ EXIT

vcp $problem problem.pddl
vcp $domain domain.pddl
plan &> log &
pid=$!

if $VERBOSE
then
    tail -f --pid $pid log
else
    wait $pid
fi

