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
  PTR=5

  Ctype  = Struct.new :type, :ptr

  def make_ptr_type(ctype)
    Ctype.new PTR, ctype
  end
  module_function :make_ptr_type

  @@ctype_int  = Ctype.new INT, nil
  @@ctype_char = Ctype.new CHAR, nil
  @@ctype_str  = Ctype.new STR, nil

  def ctype_int
   @@ctype_int
  end

  def ctype_char
   @@ctype_char
  end

  def ctype_str
   @@ctype_str
  end
  module_function :ctype_int, :ctype_char, :ctype_str
end

module Ast
  LITERAL = 0
  VAR = 1
  FUNCALL = 2
  DECL = 3
  ADDR = 4
  DEREF = 5

  @@vars_pos  = 1
  @@vars_list = Array.new
  @@strings_sid = 0
  @@strings_list = Array.new

  Uop     = Struct.new :type, :ctype, :operand
  Op      = Struct.new :type, :ctype, :left, :right
  Int     = Struct.new :type, :ctype, :ival
  Char    = Struct.new :type, :ctype, :c
  Var     = Struct.new :type, :ctype, :vname, :vpos
  Str     = Struct.new :type, :ctype, :sval, :sid
  Funcall = Struct.new :type, :ctype, :fname, :args
  Decl    = Struct.new :type, :ctype, :decl_var, :decl_init

  def make_uop(type, ctype, operand)
    Uop.new type, ctype, operand
  end

  def make_binop(type, ctype, left, right)
    Op.new type, ctype, left, right
  end

  def make_int(val)
    Int.new LITERAL, CTYPE::ctype_int, val
  end

  def make_char(c)
    Char.new LITERAL, CTYPE::ctype_char, c
  end

  def make_str(str)
    ast = Str.new LITERAL, CTYPE::ctype_str, str, @@strings_sid
    @@strings_list << ast
    @@strings_sid += 1
    ast
  end

  def make_funcall(fname, args)
    Funcall.new FUNCALL, CTYPE::ctype_int, fname, args
  end

  def make_decl(var, init)
    Decl.new DECL, nil, var, init
  end

  module_function :make_uop, :make_binop, :make_int, :make_char, :make_str, :make_funcall, :make_decl

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

def assert(expr)
  raise RuntimeError, expr unless expr
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

def result_type_int(a, b)
  swapped = false
  catch :err do
    if a.type == CTYPE::PTR
      unless b.type == CTYPE::PTR
        throw :err
      end
      return CTYPE::Ctype.new CTYPE::PTR, result_type_int(a,b)
    end

    if a.type > b.type
      swapped = true
      a,b=b,a
    end

    case a.type
    when CTYPE::VOID
      throw :err
    when CTYPE::INT
      case b.type
      when CTYPE::INT
        return CTYPE::ctype_int
      when CTYPE::CHAR
        return CTYPE::ctype_int
      when CTYPE::STR;
        throw :err
      end
      error("internal error1")
    when CTYPE::CHAR
      case b.type
      when CTYPE::CHAR
        return CTYPE::ctype_int
      when CTYPE::STR
        catch :err
      end
      error("internal error2")
    when CTYPE::STR
      throw :err
    else
      error("internal error3")
    end
  end
#err:
  a,b = b,a if (swapped)
  error("incompatible operands: %s and %s for %c"%
        [ctype_to_string(a), ctype_to_string(b)])
end

def result_type(op, a, b)
  return result_type_int(a.ctype, b.ctype)
end

def ensure_lvalue(ast)
  if (ast.type != Ast::VAR)
    error("lvalue expected, but got %s"%ast_to_string(ast))
  end
end

def read_unary_expr
  tok = read_token()
  if is_punct(tok, '&')
    operand = read_unary_expr()
    ensure_lvalue(operand)
    return Ast::make_uop(Ast::ADDR, CTYPE::make_ptr_type(operand.ctype), operand)
  end
  if is_punct(tok, '*')
    operand = read_unary_expr()
    if operand.ctype.type != CTYPE::PTR
      error("pointer type expected, but got %s"%ast_to_string(operand))
    end
    return Ast::make_uop(Ast::DEREF, operand.ctype.ptr, operand)
  end
  unget_token(tok)
  return read_prim()
end

def read_expr(prec)
  ast = read_unary_expr()
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
    ast   = Ast::make_binop(tok.punct, ctype, ast, rest)
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
    return nil
  end
  if (tok.sval == "int")
    return CTYPE::ctype_int
  end
  if (tok.sval == "char")
    return CTYPE::ctype_char
  end
  if (tok.sval == "string")
    return CTYPE::ctype_str
  end
  return nil
end

def is_type_keyword(tok)
  return get_ctype(tok) != nil
end

def expect(punct)
  tok = read_token()
  if (!is_punct(tok, punct))
    error("'%c' expected, but got %s"%[punct, token_to_string(tok)])
  end
end

def read_decl
  ctype = get_ctype(read_token())
  while true
    tok = read_token()
    break unless is_punct(tok, '*')
    ctype = CTYPE::make_ptr_type(ctype)
  end
  error("Identifier expected, but got %s"%token_to_string(tok)) if tok.type != Token::IDENT
  var = Ast::make_var(ctype, tok.sval)
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
  printf("mov %%rax, -%d(%%rbp)\n\t", var.vpos * 8)
end

def emit_binop(ast)
  if ast.type == '='
    emit_assign(ast.left, ast.right)
    return
  end
  case ast.type
  when '+' ; op = "add"
  when '-' ; op = "sub"
  when '*' ; op = "imul"
  when '/' ;
  else       error("invalid operand")
  end
  emit_expr(ast.left)
  printf("push %%rax\n\t")
  emit_expr(ast.right)
  if ast.type == '/'
    printf("mov %%rax, %%rbx\n\t")
    printf("pop %%rax\n\t")
    printf("mov $0, %%edx\n\t")
    printf("idiv %%rbx\n\t")
  else
    printf("pop %%rbx\n\t")
    printf("%s %%rbx, %%rax\n\t", op)
  end
end

def emit_expr(ast)
  case ast.type
  when Ast::LITERAL
    case ast.ctype.type
    when CTYPE::INT
      printf("mov $%d, %%rax\n\t", ast.ival)
    when CTYPE::CHAR
      printf("mov $%d, %%rax\n\t", ast.c)
    when CTYPE::STR
      printf("lea .s%d(%%rip), %%rax\n\t", ast.sid)
    end
  when Ast::VAR
    printf("mov -%d(%%rbp), %%rax\n\t", ast.vpos * 8)
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
    printf("mov $0, %%rax\n\t")
    printf("call %s\n\t", ast.fname)
    for arg in ast.args[0..-2].reverse do
      printf("pop %%%s\n\t", REGS[ast.args.index(arg)])
    end
  when Ast::DECL
    emit_assign(ast.decl_var, ast.decl_init)
  when Ast::ADDR
    assert(ast.operand.type == Ast::VAR)
    printf("lea -%d(%%rbp), %%rax\n\t", ast.operand.vpos * 8)
  when Ast::DEREF
    assert(ast.operand.ctype.type == CTYPE::PTR)
    emit_expr(ast.operand)
    printf("mov (%%rax), %%rax\n\t")
  else
    emit_binop(ast)
  end
end

def ctype_to_string(ctype)
  case (ctype.type)
  when CTYPE::VOID; return "void"
  when CTYPE::INT ; return "int"
  when CTYPE::CHAR; return "char"
  when CTYPE::STR ; return "string"
  when CTYPE::PTR ; return "%s*"%ctype_to_string(ctype.ptr)
  else error("Unknown ctype: %d", ctype.type)
  end
end

def ast_to_string_int(ast, buf)
  case ast.type
  when Ast::LITERAL
    case ast.ctype.type
    when CTYPE::INT
      buf << ast.ival.to_s
    when CTYPE::CHAR
      buf << "'%c'" % ast.c
    when CTYPE::STR
      buf << "\"%s\""%ast.sval
    end
  when Ast::VAR
    buf << ast.vname
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
  when Ast::ADDR
    buf << "(& %s)"%ast_to_string(ast.operand)
  when Ast::DEREF
    buf << "(* %s)"%ast_to_string(ast.operand)
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
           "mymain:\n\t" +
           "push %%rbp\n\t" +
           "mov %%rsp, %%rbp\n\t")
    unless Ast::vars.empty?
      var = Ast::vars[-1]
      printf("sub $%d, %%rsp\n\t", var.vpos * 8)
    end
  end
  for ast in exprs do
    if wantast
      printf("%s", ast_to_string(ast))
    else
      emit_expr(ast)
    end
  end
  if !wantast
    printf("leave\n\t" +
           "ret\n")
  end
end
