#!/usr/bin/ruby

require "scanf"

if __FILE__ == $0

   val = scanf("%d")[0]
   print(<<EOS)
    .text
    .global mymain
    mymain:
    mov $#{val}, %rax
    ret
EOS
   
end
