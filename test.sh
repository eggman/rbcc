#!/bin/bash

function compile {
  echo "$1" | ruby ./rbcc.rb > tmp.s
  if [ $? -ne 0 ]; then
    echo "Failed to compile $1"
    echo $1 $2
    exit
  fi
  gcc -o tmp.out driver.c tmp.s
  if [ $? -ne 0 ]; then
    echo "GCC failed $1"
    echo $1 $2
    exit
  fi
}


function assertequal {
  if [ "$1" != "$2" ]; then
    echo "Test failed: $2 expected but got $1"
  compile "$expr"
    exit
  fi
}

function testast {
  result="$(echo "$2" | ruby ./rbcc.rb -a)"
  if [ $? -ne 0 ]; then
    echo "Failed to compile $1"
    exit
  fi
  assertequal "$result" "$1"
}

function test {
  compile "$2"
  assertequal "$(./tmp.out)" "$1"
}

function testfail {
  expr="int f(){$1}"
  echo "$expr" | ruby ./rbcc.rb > /dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "Should fail to compile, but succeded: $expr"
    exit
  fi
}

testast '1' '1;'
testast '(+ (- (+ 1 2) 3) 4)' '1+2-3+4;'
testast '(+ (+ 1 (* 2 3)) 4)' '1+2*3+4;'
testast '(+ (* 1 2) (* 3 4))' '1*2+3*4;'
testast '(+ (/ 4 2) (/ 6 3))' '4/2+6/3;'
testast '(/ (/ 24 2) 4)' '24/2/4;'

testast '(= a 3)' 'a=3;'

testast 'a()' 'a();'
testast 'a(b,c,d,e,f,g)' 'a(b,c,d,e,f,g);'

test 0 '0;'

test 3 '1+2;'
test 3 '1 + 2;'
test 10 '1+2+3+4;'
test 11 '1+2*3+4;'
test 14 '1*2+3*4;'
test 4 '4/2+6/3;'
test 3 '24/2/4;'

test 2 '1;2;'
test 3 'a=1;a+2;'
test 102 'a=1;b=48+2;c=a+b;c*2;'

testfail '0abc;'
testfail '1+;'

test 25 'sum2(20, 5);'
test 15 'sum5(1, 2, 3, 4, 5);'

echo "All tests passed"
