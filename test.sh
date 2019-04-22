#!/bin/sh
set -e

success=0
failure=0
verbosity=${VERBOSITY:=0}
option=${OPTION:=}

for testcase in input/*.ml;
do
  set +e
  if [ $verbosity = 0 ]; then
    ./minimlc $option $testcase > /dev/null
  elif [ $verbosity = 1 ]; then
    ./minimlc $option $testcase
  else
    ./minimlc $option -v $testcase
  fi
  result=$?
  set -e

  if [ $result = 0 ]; then
    echo "$testcase \033[32msuccess\033[m"
    success=$(($success + 1))
  else
    echo "$testcase \033[31mfailure\033[m";
    failure=$(($failure + 1))
  fi
done

echo "done. success: $success failure: $failure"
[ $failure -eq 0 ]
