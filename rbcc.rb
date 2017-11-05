#!/usr/bin/ruby

load 'string.rb'
load 'lex.rb'

MAX_ARGS=6
REGS=["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

module CTYPE
  VOID=0
  INT=1
  CHAR=2
  STR=4
end

module Ast
  INT = 0
  CHAR = 1
  VAR = 2
  STR = 3
  FUNCALL = 4
  DECL = 5

  @@vars_pos  = 0
  @@vars_list = Array.new
  @@strings_sid = 0
  @@strings_list = Array.new

  Op      = Struct.new :type, :ctype, :left, :right
  Int     = Struct.new :type, :ctype, :ival
  Char    = Struct.new :type, :ctype, :c
  Var     = Struct.new :type, :ctype, :vname, :vpos
  Str     = Struct.new :type, :ctype, :sval, :sid
  Funcall = Struct.new :type, :fname, :args
  Decl    = Struct.new :type, :decl_var, :decl_init

  def make_op(type, ctype, left, right)
    Op.new type, ctype, left, right
  end

  def make_int(val)
    Int.new INT, CTYPE::INT, val
  end

  def make_char(c)
    Char.new CHAR, CTYPE::CHAR,  c
  end

  def make_str(str)
    ast = Str.new STR, CTYPE::STR, str, @@strings_sid
    @@strings_list << ast
    @@strings_sid += 1
    ast
  end

  def make_funcall(fname, args)
    Funcall.new FUNCALL, fname, args
  end

  def make_decl(var, init)
    Decl.new DECL, var, init
  end

  module_function :make_op, :make_int, :make_char, :make_str, :make_funcall, :make_decl

  def strings
    @@strings_list
  end
  module_function :strings

  def find_var(name)
    @@vars_list.each {|var|
      return var if (var.vname == name)
    }
    return nil
  end
  module_function :find_var

  def make_var (ctype, vname)
    unless find_var(vname)
      var = Var.new VAR, ctype, vname, @@vars_pos
      @@vars_pos += 1
      @@vars_list << var
    else
      var = find_var(vname)
    end
    return var
  end
  module_function :make_var

  def vars
    @@vars_list
  end
  module_function :vars
end

def error(str)
  raise RuntimeError, str
end

def is_right_assoc(op)
  return op == '='
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

def read_func_args(fname)
  args = Array.new
  for i in 1..MAX_ARGS do
    tok = read_token()
    break if is_punct(tok, ')')
    unget_token(tok)
    args<<read_expr(0)
    tok = read_token()
    break if is_punct(tok, ')')
    error("Unexpected character: '%s'"%token_to_string(tok)) unless is_punct(tok, ',')
  end
  if args.length > MAX_ARGS
    error("Too many arguments: %s", fname)
  end
  return Ast::make_funcall(fname, args)
end

def read_ident_or_func(name)
  tok = read_token()
  if is_punct(tok, '(')
   return read_func_args(name)
  end
  unget_token(tok)
  v = Ast::find_var(name)
  if(!v)
    error("Undefined varaible: %s"%name)
  end
  return v
end

def read_prim
  tok = read_token()
  return nil unless tok
  case tok.type
  when Token::IDENT ; return read_ident_or_func(tok.sval)
  when Token::INT   ; return Ast::make_int(tok.ival)
  when Token::CHAR  ; return Ast::make_char(tok.c)
  when Token::STRING; return Ast::make_str(tok.sval)
  when Token::PUNCT ; error("unexpected character: '%c'"%tok.punct);
  else
    error("internal error: unknown token type: %d"%tok.type)
  end
end

def result_type(op, a, b)
  swapped = false
  if a.ctype > b.ctype
    swapped = true
    a,b=b,a
  end
  catch :err do
    case a.ctype
    when CTYPE::VOID;
      throw :err
    when CTYPE::INT;
      case b.ctype
      when CTYPE::INT;
        return CTYPE::INT
      when CTYPE::CHAR;
        return CTYPE::INT
      when CTYPE::STR;
        throw :err
      end
      error("internal error1")
    when CTYPE::CHAR;
      case b.ctype
      when CTYPE::CHAR;
        return CTYPE::INT
      when CTYPE::STR;
        catch :err
      end
      error("internal error2")
    when CTYPE::STR;
      throw :err
    else
      error("internal error3")
    end
  end
#err:
  a,b = b,a if (swapped)
  error("incompatible operands: %s and %s for %c"%
        [ast_to_string(a), ast_to_string(b), op])
end

def read_expr(prec)
  ast = read_prim()
  return nil unless ast
  while true
    tok = read_token()
    unless tok.type == Token::PUNCT
      unget_token(tok)
      return ast
    end
    prec2 = priority(tok.punct)
    if prec2 < prec
      unget_token(tok)
      return ast
    end
    rest  = read_expr(prec2 + (is_right_assoc(tok.punct) ? 0 : 1))
    ctype = result_type(tok.punct, ast, rest)
    ast   = Ast::make_op(tok.punct, ctype, ast, rest)
  end
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

def get_ctype(tok)
  if (tok.type != Token::IDENT)
    return -1
  end
  if (tok.sval == "int")
    return CTYPE::INT
  end
  if (tok.sval == "char")
    return CTYPE::CHAR
  end
  if (tok.sval == "string")
    return CTYPE::STR
  end
  return -1
end

def is_type_keyword(tok)
  return get_ctype(tok) != -1
end

def expect(punct)
  tok = read_token()
  if (!is_punct(tok, punct))
    error("'%c' expected, but got %s"%[punct, token_to_string(tok)])
  end
end

def read_decl
  ctype = get_ctype(read_token())
  name = read_token()
  error("Identifier expected, but got %s"%token_to_string(name)) if name.type != Token::IDENT
  var = Ast::make_var(ctype, name.sval)
  expect('=')
  init = read_expr(0)
  return Ast::make_decl(var, init)
end

def read_decl_or_stmt
  tok = peek_token()
  return nil unless tok
  r = is_type_keyword(tok) ? read_decl() : read_expr(0)
  tok = read_token()
  error("Unterminated expression: %s"%token_to_string(tok)) unless is_punct(tok, ';')
  return r
end

def emit_assign(var, value)
  emit_expr(value)
  printf("mov %%eax, -%d(%%rbp)\n\t", var.vpos * 4)
end

def emit_binop(ast)
  if ast.type == '='
    emit_assign(ast.left, ast.right)
    return
  end
  if ast.type == Ast::VAR || ast.type == Ast::INT
    error("invalid operand")
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
  when Ast::VAR
    printf("mov -%d(%%rbp), %%eax\n\t", ast.vpos * 4)
  when Ast::CHAR then
    printf("mov $%d, %%eax\n\t", ast.c)
  when Ast::STR
    printf("lea .s%d(%%rip), %%rax\n\t", ast.sid)
  when Ast::FUNCALL
    for i in 1..(ast.args.length-1) do
      printf("push %%%s\n\t", REGS[i])
    end
    for arg in ast.args do
      emit_expr(arg)
      printf("push %%rax\n\t")
    end
    for arg in ast.args.reverse do
      printf("pop %%%s\n\t", REGS[ast.args.index(arg)])
    end
    printf("mov $0, %%eax\n\t")
    printf("call %s\n\t", ast.fname)
    for arg in ast.args[0..-2].reverse do
      printf("pop %%%s\n\t", REGS[ast.args.index(arg)])
    end
  when Ast::DECL
    emit_assign(ast.decl_var, ast.decl_init)
  else
    emit_binop(ast)
  end
end

def ctype_to_string(ctype)
  case (ctype)
  when CTYPE::VOID; return "void"
  when CTYPE::INT ; return "int"
  when CTYPE::CHAR; return "char"
  when CTYPE::STR ; return "string"
  else error("Unknown ctype: %d", ctype)
  end
end

def ast_to_string_int(ast, buf)
  case ast.type
  when Ast::INT
    buf << ast.ival.to_s
  when Ast::VAR
    buf << ast.vname
  when Ast::CHAR
    buf << "'%c'" % ast.c
  when Ast::STR
    buf << "\"" + ast.sval + "\""
  when Ast::FUNCALL
    buf << "%s(" % ast.fname
    for arg in ast.args do
      ast_to_string_int(arg, buf)
      buf << "," unless arg == ast.args[-1]
    end
    buf << ")"
  when Ast::DECL
    buf << "(decl %s %s %s)" % [
             ctype_to_string(ast.decl_var.ctype),
             ast.decl_var.vname,
             ast_to_string(ast.decl_init)]
  else
    buf << "(%c %s %s)" % [ast.type,
                           ast_to_string(ast.left),
                           ast_to_string(ast.right)]
  end
end

def ast_to_string(ast)
  s = String.new
  return ast_to_string_int(ast, s);
end

def emit_data_section
  return if Ast::strings.empty?
  printf("\t.data\n")
  for str in Ast::strings do
    printf(".s%d:\n\t", str.sid)
    printf(".string \"")
    print(str.sval)
    printf("\"\n")
  end
  printf("\t")
end

if __FILE__ == $0
  wantast = ARGV[0] == "-a"
  exprs = Array.new
  while true
    ast = read_decl_or_stmt()
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
      printf("%s", ast_to_string(ast))
    else
      emit_expr(ast)
    end
  end
  if !wantast
    printf("ret\n")
  end
end
