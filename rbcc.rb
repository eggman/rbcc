#!/usr/bin/ruby

module Ast
  INT = 0
  STR = 1

  Op    = Struct.new :type, :left, :right
  Int   = Struct.new :type, :ival
  Str   = Struct.new :type, :sval
end

def error(str)
  raise RuntimeError, str
end

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

def priority(op)
  case op
  when '+','-' then
    return 1
  when '*','/' then
    return 2
  else
    error("Unknown binary operator: %c"%op)
  end
end

def read_number(n)
  while c = STDIN.getc
    unless is_int(c)
      STDIN.ungetc(c)
      return Ast::Int.new Ast::INT, n
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
  return Ast::Str.new Ast::STR, buf
end

def read_expr2(prec)
  ast = read_prim()
  while true
    skip_space()
    c = STDIN.getc
    return ast if STDIN.eof
    prec2 = priority(c)
    if prec2 < prec
      STDIN.ungetc(c)
      return ast
    end
    skip_space()
    ast = Ast::Op.new c, ast, read_expr2(prec2 + 1)
  end
end

def read_expr
  return read_expr2(0)
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

def ensure_intexpr(ast)
  case ast.type
  when !Ast::Op && !Ast::Int
    error("integer or binary operator expected")
  end
end

def emit_binop(ast)
  if ast.type == Ast::STR || ast.type == Ast::INT
    error("invalid operand");
  elsif ast.type == "+"
    op = "add"
  elsif ast.type == "-"
    op = "sub"
  elsif ast.type == "*"
    op = "imul"
  end
  emit_intexpr(ast.left)
  printf("push %%rax\n\t")
  emit_intexpr(ast.right)
  if ast.type == '/'
    printf("mov %%eax, %%ebx\n\t")
    printf("pop %%rax\n\t")
    printf("mov $0, %%edx\n\t")
    printf("idiv %%ebx\n\t")
  else
    printf("pop %%rbx\n\t")
    printf("%s %%ebx, %%eax\n\t", op)
  end
end

def emit_intexpr(ast)
  ensure_intexpr(ast)
  if ast.type == Ast::INT
    printf("mov $%d, %%eax\n\t", ast.ival)
  else
    emit_binop(ast)
  end
end

def compile(ast)
  if ast.type == Ast::STR
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
  case ast.type
  when Ast::INT
    print ast.ival.to_s
  when Ast::STR
    print ast.sval.to_s
  else
    printf("(%c ", ast.type)
    print_ast(ast.left)
    print(" ")
    print_ast(ast.right)
    print(")")
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
