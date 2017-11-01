#!/usr/bin/ruby

def error(str)
  raise SyntaxError, str
end

def is_int(str)
  Integer(str) != nil rescue false
end
 
def compile_number(n)
  while (c = STDIN.getc)
    break if (c.strip == "")
    error("Invalid character in number: '%c'"%c) unless (is_int(c))
    n = n*10 + c.to_i
  end
  print(<<EOS)
    .text
    .global intfn
    intfn:
    mov $#{n}, %rax
    ret
EOS
end

def compile_string
  buf = ""
  while true
    c = STDIN.getc
    error("Unterminated string") if (c == "")
    break if (c == '"')
    if (c == '\\')
      c = STDIN.getc
      error("Unterminated string") if (c == "")
    end
    buf<<c
  end
  print(<<EOS)
    .data
    .mydata:
    .string "#{buf}"
    .text
    .global stringfn
    stringfn:
    lea .mydata(%rip), %rax
    ret
EOS
end

def compile
  c = STDIN.getc
  if is_int(c)
    return compile_number(c.to_i)
  end
  if  c == '"'
    return compile_string()
  end
  error("Don't know how to handle '%c'"%c)
end

if __FILE__ == $0
  compile()
end
