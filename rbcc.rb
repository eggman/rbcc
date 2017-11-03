#!/usr/bin/ruby

MAX_ARGS=6
REGS=["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

class String
  def numeric?
    Integer(self) != nil rescue false
  end
  def alnum?
    !!match(/^[[:alnum:]]+$/)
  end
  def alpha?
    !!match(/^[[:alpha:]]+$/)
  end
end

module Var
  @@vars_pos  = 0
  @@vars_list = Array.new
  Var = Struct.new :name, :pos

  def find_var(name)
    @@vars_list.each {|var|
      return var if (var.name == name)
    }
    return nil
  end
  module_function :find_var

  def make (name)
    unless find_var(name)
      var = Var.new name, @@vars_pos
      @@vars_pos += 1
      @@vars_list<<var
    else
      var = find_var(name)
    end
    return var
  end
  module_function :make
end

module Ast
  INT = 0
  SYM = 1
  STR = 2
  FUNCALL = 3

  @@strings_sid = 0
  @@strings_list = Array.new

  Op      = Struct.new :type, :left, :right
  Int     = Struct.new :type, :ival
  Sym     = Struct.new :type, :var
  Str     = Struct.new :type, :sval, :sid
  Funcall = Struct.new :type, :fname, :args

  def make_str(str)
    ast = Str.new Ast::STR, str, @@strings_sid
    @@strings_list << ast
    @@strings_sid += 1
    ast
  end
  module_function :make_str

  def strings
    @@strings_list
  end
  module_function :strings

end

def error(str)
  raise RuntimeError, str
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
  when '='
    return 1
  when '+', '-'
    return 2
  when '*', '/'
    return 3
  else
    return -1
  end
end

def read_number(n)
  while c = STDIN.getc
    unless c.numeric?
      STDIN.ungetc(c)
      return Ast::Int.new Ast::INT, n
    end
    n = n * 10 + c.to_i
  end
end

def read_ident(c)
  buf = c
  while true
    c = STDIN.getc
    unless c.alnum?
      STDIN.ungetc(c)
      break
    end
    buf << c
  end
  return buf
end

def read_func_args(fname)
  args = Array.new
  for i in 1..MAX_ARGS do
    skip_space()
    c = STDIN.getc
    break if c == ')'
    STDIN.ungetc(c)
    args<<read_expr2(0)
    c = STDIN.getc
    break if c == ')'
    if c == ','
      skip_space()
    else
      error("Unexpected character: '%c'"%c)
    end
  end
  if args.length > MAX_ARGS
    error("Too many arguments: %s", fname);
  end
  return Ast::Funcall.new Ast::FUNCALL, fname, args
end

def read_ident_or_func(c)
  name = read_ident(c)
  skip_space()
  c2 = STDIN.getc
  if c2 == '('
    return read_func_args(name)
  end
  STDIN.ungetc(c2)
  var = Var::make(name)
  return Ast::Sym.new Ast::SYM, var
end

def read_prim
  return nil                 if STDIN.eof
  c = STDIN.getc
  return read_number(c.to_i) if c.numeric?
  return read_string()         if c == '"'
  return read_ident_or_func(c) if c.alpha?
  error("Don't know how to handle '%c'."%c)
end

def read_string
  buf = ""
  while true
    error("Unterminated string") if STDIN.eof
    c = STDIN.getc
    break if c == '"'
    if c == '\\'
      error("Unterminated string") if STDIN.eof
      c = STDIN.getc
    end
    buf << c
  end
  return Ast::make_str(buf)
end

def read_expr2(prec)
  skip_space()
  ast = read_prim()
  return nil unless ast
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
  r = read_expr2(0)
  return nil unless r
  skip_space()
  c = STDIN.getc
  error("Unterminated expression") unless c == ';'
  return r
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

def emit_binop(ast)
  if ast.type == '='
    emit_expr(ast.right)
    unless ast.left.type == Ast::SYM
      error("Symbol expected")
    end
    printf("mov %%eax, -%d(%%rbp)\n\t", ast.left.var.pos * 4)
    return
  end
  if ast.type == Ast::SYM || ast.type == Ast::INT
    error("invalid operand");
  elsif ast.type == "+"
    op = "add"
  elsif ast.type == "-"
    op = "sub"
  elsif ast.type == "*"
    op = "imul"
  end
  emit_expr(ast.left)
  printf("push %%rax\n\t")
  emit_expr(ast.right)
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

def emit_expr(ast)
  case ast.type
  when Ast::INT
    printf("mov $%d, %%eax\n\t", ast.ival)
  when Ast::SYM
    printf("mov -%d(%%rbp), %%eax\n\t", ast.var.pos * 4)
  when Ast::STR
    printf("lea .s%d(%%rip), %%rax\n\t", ast.sid);
  when Ast::FUNCALL
    for i in 1..(ast.args.length-1) do
      printf("push %%%s\n\t", REGS[i])
    end
    for arg in ast.args do
      emit_expr(arg)
      printf("push %%rax\n\t");
    end
    for arg in ast.args.reverse do
      printf("pop %%%s\n\t", REGS[ast.args.index(arg)])
    end
    printf("mov $0, %%eax\n\t");
    printf("call %s\n\t", ast.fname);
    for arg in ast.args[0..-2].reverse do
      printf("pop %%%s\n\t", REGS[ast.args.index(arg)])
    end
 else
    emit_binop(ast)
  end
end

def print_ast(ast)
  case ast.type
  when Ast::INT
    print ast.ival.to_s
  when Ast::SYM
    print ast.var.name
  when Ast::STR then
    printf("\"")
    print(ast.sval)
    printf("\"")
  when Ast::FUNCALL then
    printf("%s(", ast.fname);
    for arg in ast.args do
      print_ast(arg)
      printf(",") unless arg == ast.args[-1]
    end
    print(")")
  else
    printf("(%c ", ast.type)
    print_ast(ast.left)
    print(" ")
    print_ast(ast.right)
    print(")")
  end
end

def emit_data_section
  return if Ast::strings.empty?
  printf("\t.data\n")
  for str in Ast::strings do
    printf(".s%d:\n\t", str.sid);
    printf(".string \"");
    print(str.sval);
    printf("\"\n");
  end
  printf("\t")
end

if __FILE__ == $0
  wantast = ARGV[0] == "-a"
  exprs = Array.new
  while true
    ast = read_expr()
    break unless ast
    exprs << ast
  end
  if !wantast
    emit_data_section()
    printf(".text\n\t" +
           ".global mymain\n" +
           "mymain:\n\t")
  end
  for ast in exprs do
    if wantast
      print_ast(ast)
    else
      emit_expr(ast)
    end
  end
  if !wantast
    printf("ret\n")
  end
end
