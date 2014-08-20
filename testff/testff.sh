#!/bin/bash

## ff
# this should finish normally (because it is easy)
../clean.sh
../macroff-clean p01.pddl domain.pddl

../clean.sh
../macroff-clean -t 1 p01.pddl domain.pddl

# this should fail (because soft > hard)
../clean.sh
../macroff-clean -T 1 p01.pddl domain.pddl

../clean.sh
../macroff-clean -t 1 -T 30 p02.pddl domain.pddl

# force hard limit
../clean.sh
../macroff-clean -t 3 -T 5 p03.pddl domain.pddl

# increased verbosity
../clean.sh
../macroff-clean -v -t 1 p01.pddl domain.pddl

