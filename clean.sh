#! /bin/bash

echo "##### START CLEANING #############################################"

pushd ${1:-$(pwd)}
pwd
rm -fv *.sas *.sasp *.cost *.log *.time *.plan.* downward.tmp.*
popd

echo "##### END CLEANING #############################################"

