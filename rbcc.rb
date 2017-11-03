#!/usr/bin/ruby

class String
  def numeric?
    Integer(self) != nil rescue false
  end
  def alpha?
    !!match(/^[[:alnum:]]+$/)
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

  Op    = Struct.new :type, :left, :right
  Int   = Struct.new :type, :ival
  Sym   = Struct.new :type, :var
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

def read_symbol(c)
  buf = c
  while true
    c = STDIN.getc
    unless c.alpha?
      STDIN.ungetc(c)
      break
    end
    buf<<c
  end
  var = Var::make(buf)
  return Ast::Sym.new Ast::SYM, var
end

def read_prim
  return nil                 if STDIN.eof
  c = STDIN.getc
  return read_number(c.to_i) if c.numeric?
  return read_symbol(c)      if c.alpha?
  error("Don't know how to handle '%c'."%c)
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
  else
    emit_binop(ast)
  end
end

def print_ast(ast)
  case ast.type
  when Ast::INT
    print ast.ival.to_s
  when Ast::SYM
    print ast.var.name.to_s
  else
    printf("(%c ", ast.type)
    print_ast(ast.left)
    print(" ")
    print_ast(ast.right)
    print(")")
  end
end

if __FILE__ == $0
  wantast = ARGV[0] == "-a"
  if !wantast
    printf(".text\n\t" +
           ".global mymain\n" +
           "mymain:\n\t")
  end
  while true
    ast = read_expr()
    break unless ast
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
