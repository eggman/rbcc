#!/usr/bin/ruby

def error(str)
  raise SyntaxError, str
end

def is_int(str)
  Integer(str) != nil rescue false
end

def skip_space
  while (c = STDIN.getc)
    next if (c.strip == "")
    STDIN.ungetc(c)
    return
  end
end

def read_number(n)
  while (c = STDIN.getc)
    unless (is_int(c))
      STDIN.ungetc(c)
      return n
    end
    n = n * 10 + c.to_i
  end
end

def compile_expr2()
  while true
    skip_space()
    c = STDIN.getc
    if (STDIN.eof)
      print("ret\n")
      exit
    end

    if (c == '+')
      op = "add"
    elsif (c == '-')
      op = "sub"
    else
      error("Operator expected, but got '%c'"%c)
    end
    skip_space()
    c = STDIN.getc
    error("Number expected, but got '%c'"%c) unless (is_int(c))
    printf("    %s $%d, %%rax\n    ", op, read_number(c.to_i));
  end
end
 
def compile_expr(n)
  n = read_number(n)
  print(<<EOS)
    .text
    .global intfn
    intfn:
    mov $#{n}, %rax
EOS
  compile_expr2()
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
  exit
end

def compile
  c = STDIN.getc
  if is_int(c)
    compile_expr(c.to_i)
  elsif  c == '"'
    compile_string()
  else
    error("Don't know how to handle '%c'"%c)
  end
end

if __FILE__ == $0
  compile()
end
