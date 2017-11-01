#!/bin/bash

function test {
  expected="$1"
  expr="$2"

  echo "$expr" | ruby ./rbcc.rb > tmp.s
  if [ ! $? ]; then
    echo "Failed to compile $expr"
    exit
  fi
  gcc -o tmp.out driver.c tmp.s || exit
  result="`./tmp.out`"
  if [ "$result" != "$expected" ]; then
    echo "Test failed: $expected expected but got $result"
    exit
  fi
}

test 0 0
test 42 42

echo "All tests passed"
