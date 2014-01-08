#! /bin/bash

../clean.sh
../test-problem.sh model2a1.pddl domain.pddl &
SCR_PID=$!
sleep 0.5
echo "test.sh: !!!! killing $SCR_PID !!!!"
kill $SCR_PID
../clean.sh