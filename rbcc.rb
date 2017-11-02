#!/usr/bin/ruby

module Ast
  Op    = Struct.new :op, :left, :right
  Int   = Struct.new :ival
  Str   = Struct.new :sval
end

def error(str)
  raise RuntimeError, str
1end

def is_int(str)
  Integer(str) != nil rescue false
end

def skip_space
  while c = STDIN.getc
    next if c.strip == ""
    STDIN.ungetc(c)
    return
  end
end

def read_number(n)
  while c = STDIN.getc
    unless is_int(c)
      STDIN.ungetc(c)
      return Ast::Int.new n
    end
    n = n * 10 + c.to_i
  end
end

def read_prim
  c = STDIN.getc
  return read_number(c.to_i) if is_int(c)
  return read_string()       if c == '"'
  error("Unexpedted EOF")    if STDIN.eof
  error("Don't know how to handle '%c'."%c)
end

def read_string
  buf = ""
  while true
    c = STDIN.getc
    error("Unterminated string") if STDIN.eof
    break if c == '"'
    if c == '\\'
      c = STDIN.getc
    end
    buf<<c
  end
  return Ast::Str.new buf
end

def read_expr2(left)
  skip_space()
  c = STDIN.getc
  return left if STDIN.eof
  if c == '+'
    op = '+'
  elsif c == '-'
    op = '-'
  else
    error("Operator expected, but got '%c'."%c)
  end
  skip_space()
  right = read_prim()
  return read_expr2(Ast::Op.new op, left, right)
end

def read_expr
  left = read_prim()
  return read_expr2(left)
end

def emit_string(ast)
  printf("\t.data\n" +
         ".mydata:\n\t" +
         ".string \"")
  print(ast.sval)
  printf("\"\n\t" +
         ".text\n\t" +
         ".global stringfn\n" +
         "stringfn:\n\t" +
         "lea .mydata(%%rip), %%rax\n\t" +
         "ret\n")
end

def ensureintexpr(ast)
  case ast.class
  when !Ast::Op && !Ast::Int
    error("integer or binary operator expected")
  end
end

def emit_binop(ast)
  if    ast.class == Ast::Op && ast.op == "+"
    op = "add"
  elsif ast.class == Ast::Op && ast.op == "-"
    op = "sub"
  else
    error("invalid operand");
  end
  emit_intexpr(ast.left)
  printf("mov %%eax, %%ebx\n\t")
  emit_intexpr(ast.right)
  printf("%s %%ebx, %%eax\n\t", op)
end

def emit_intexpr(ast)
  ensureintexpr(ast)
  if ast.class == Ast::Int
    printf("mov $%d, %%eax\n\t", ast.ival)
  else
    emit_binop(ast)
  end
end

def compile(ast)
  if ast.class == Ast::Str
    emit_string(ast)
  else
    printf(".text\n\t"+
           ".global intfn\n" +
           "intfn:\n\t")
    emit_intexpr(ast)
    printf("ret\n")
  end
end

def print_ast(ast)
  if ast.class == Ast::Op
    print("(+ ") if ast.op == "+"
    print("(- ") if ast.op == "-"
    print_ast(ast.left)
    print(" ")
    print_ast(ast.right)
    print(")")
  elsif ast.class == Ast::Int
    print ast.ival.to_s
  elsif ast.class == Ast::Str
    print ast.sval.to_s
  end
end

if __FILE__ == $0
  ast = read_expr()
  if ARGV[0] == "-a"
    print_ast(ast)
  else
    compile(ast)
  end
end
