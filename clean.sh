#! /bin/bash

pushd ${1:-$(pwd)}
pwd
rm -fv *.sas *.sasp *.cost *.log *.time *.plan.* downward.tmp.*
popd
