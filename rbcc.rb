#!/usr/bin/ruby

load 'util.rb'
load 'string.rb'
load 'lex.rb'

MAX_ARGS=6
REGS=["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

module CTYPE
  VOID=0
  INT=1
  CHAR=2
  ARRAY=4
  PTR=5

  Ctype  = Struct.new :type, :ptr, :size

  def make_array_type(ctype, size)
    Ctype.new ARRAY, ctype, size
  end

  def make_ptr_type(ctype)
    Ctype.new PTR, ctype, 0
  end
  module_function :make_array_type, :make_ptr_type

  @@ctype_int  = Ctype.new INT, nil
  @@ctype_char = Ctype.new CHAR, nil
  @@ctype_array  = Ctype.new ARRAY, nil

  def ctype_int
    @@ctype_int
  end

  def ctype_char
    @@ctype_char
  end

  def ctype_array
    @@ctype_array
  end
  module_function :ctype_int, :ctype_char, :ctype_array
end

module Ast
  LITERAL = 0
  STRING = 1
  LVAR = 2
  LREF = 3
  GVAR = 4
  GREF = 5
  FUNCALL = 6
  DECL = 7
  ARRAY_INIT = 8
  ADDR = 9
  DEREF = 10

  @@globals_list = Array.new
  @@locals_list = Array.new
  @@labelseq = 0

  Lvar    = Struct.new :type, :ctype, :lname, :loff
  Gvar    = Struct.new :type, :ctype, :gname
  Lref    = Struct.new :type, :ctype, :lref, :lrefoff
  Gref    = Struct.new :type, :ctype, :gref, :goff
  Uop     = Struct.new :type, :ctype, :operand
  Op      = Struct.new :type, :ctype, :left, :right
  Int     = Struct.new :type, :ctype, :ival
  Char    = Struct.new :type, :ctype, :c
  Var     = Struct.new :type, :ctype, :vname, :vpos
  Str     = Struct.new :type, :ctype, :sval, :slabel
  Funcall = Struct.new :type, :ctype, :fname, :args
  Decl    = Struct.new :type, :ctype, :decl_var, :decl_init
  ArrayInit = Struct.new :type, :ctype, :size, :array_init

  def ast_lvar(ctype, name)
    ast = Lvar.new LVAR, ctype, name, 0
    @@locals_list << ast
    return ast
  end

  def ast_gvar(ctype, name)
    ast = Gvar.new GVAR, ctype, name
    @@globals_list << ast
    return ast
  end

  def ast_lref(ctype, lvar, off)
    Lref.new LREF, ctype, lvar, off
  end

  def ast_gref(ctype, gvar, off)
    Gref.new GREF, ctype, gvar, off
  end

  def ast_uop(type, ctype, operand)
    Uop.new type, ctype, operand
  end

  def ast_binop(type, ctype, left, right)
    Op.new type, ctype, left, right
  end

  def ast_int(val)
    Int.new LITERAL, CTYPE::ctype_int, val
  end

  def ast_char(c)
    Char.new LITERAL, CTYPE::ctype_char, c
  end

  def ast_string(str)
    ast = Str.new STRING, CTYPE::make_array_type(CTYPE::ctype_char, str.length + 1), str, make_next_label()
    @@globals_list << ast
    ast
  end

  def ast_funcall(fname, args)
    Funcall.new FUNCALL, CTYPE::ctype_int, fname, args
  end

  def ast_decl(var, init)
    Decl.new DECL, nil, var, init
  end

  def ast_array_init(size, array_init)
    ArrayInit.new ARRAY_INIT, nil, size, array_init
  end

  module_function :ast_lvar, :ast_gvar, :ast_lref, :ast_gref, :ast_uop, :ast_binop, :ast_int, :ast_char, :ast_string, :ast_funcall, :ast_decl, :ast_array_init

  def make_next_label
    s = ".L%d"%@@labelseq
    @@labelseq += 1
    return s
  end
  module_function :make_next_label

  def find_var(name)
    @@locals_list.each {|var|
      return var if (var.lname == name)
    }
    @@globals_list.each {|var|
      return var if (var.gname == name)
    }
    return nil
  end
  module_function :find_var

  def locals
    @@locals_list
  end
  module_function :locals

  def globals
    @@globals_list
  end
  module_function :globals

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
  return Ast::ast_funcall(fname, args)
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
  when Token::INT   ; return Ast::ast_int(tok.ival)
  when Token::CHAR  ; return Ast::ast_char(tok.c)
  when Token::STRING; return Ast::ast_string(tok.sval)
  when Token::PUNCT ; error("unexpected character: '%c'"%tok.punct);
  else
    error("internal error: unknown token type: %d"%tok.type)
  end
end

def result_type_int(op, a, b)
  swapped = false
  catch :err do
    if a.type > b.type
      swapped = true
      a,b=b,a
    end

    if b.type == CTYPE::PTR
      if op != '+' && op != '-'
        throw :err
      end
      unless a.type == CTYPE::INT
        throw :err
      end
      return b
    end

    case a.type
    when CTYPE::VOID
      throw :err
    when CTYPE::INT,
         CTYPE::CHAR
      case b.type
      when CTYPE::INT,
           CTYPE::CHAR
        return CTYPE::ctype_int
      when CTYPE::ARRAY,
           CTYPE::PTR
        return b
      end
      error("internal error");
    when CTYPE::ARRAY
      throw :err
    else
      error("internal error")
    end
  end
#err:
  a,b = b,a if (swapped)
  error("incompatible operands: %c: <%s> and <%s>"%
        op,[ctype_to_string(a), ctype_to_string(b)])
end

def result_type(op, a, b)
  return result_type_int(op, a, b)
end

def ensure_lvalue(ast)
  case ast.type
  when Ast::LVAR, Ast::LREF, Ast::GVAR, Ast::GREF
    return
  else
    error("lvalue expected, but got %s"%ast_to_string(ast))
  end
end

def read_unary_expr
  tok = read_token()
  if is_punct(tok, '&')
    operand = read_unary_expr()
    ensure_lvalue(operand)
    return Ast::ast_uop(Ast::ADDR, CTYPE::make_ptr_type(operand.ctype), operand)
  end
  if is_punct(tok, '*')
    operand = read_unary_expr()
    if operand.ctype.type != CTYPE::PTR
      error("pointer type expected, but got %s"%ast_to_string(operand))
    end
    return Ast::ast_uop(Ast::DEREF, operand.ctype.ptr, operand)
  end
  unget_token(tok)
  return read_prim()
end

def convert_array(ast)
  if ast.type == Ast::STRING
    return Ast::ast_gref(CTYPE::make_ptr_type(CTYPE::ctype_char), ast, 0)
  end
  if ast.ctype.type != CTYPE::ARRAY
    return ast
  end
  if ast.type == Ast::LVAR
    return Ast::ast_lref(CTYPE::make_ptr_type(ast.ctype.ptr), ast, 0);
  end
  if ast.type != Ast::GVAR
    error("Internal error: Gvar expected, but got %s"%ast_to_string(ast))
  end
  return Ast::ast_gref(CTYPE::make_ptr_type(ast.ctype.ptr), ast, 0)
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
    if is_punct(tok, '=')
      ensure_lvalue(ast)
    else
      ast = convert_array(ast)
    end
    rest  = read_expr(prec2 + (is_right_assoc(tok.punct) ? 0 : 1))
    rest  = convert_array(rest)
    ctype = result_type(tok.punct, ast.ctype, rest.ctype)
    if !is_punct(tok, '=') &&
       ast.ctype.type != CTYPE::PTR &&
       rest.ctype.type == CTYPE::PTR
       ast, rest = rest, ast
    end
    ast   = Ast::ast_binop(tok.punct, ctype, ast, rest)
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

def read_decl_array_initializer(ctype)
  tok = read_token()
  if ctype.ptr.type == CTYPE::CHAR && tok::type == Token::STRING
    return Ast::ast_string(tok.sval)
  end
  if !is_punct(tok, '{')
    error("Expected an initializer list, but got %s", token_to_string(tok));
  end

  init = Array.new
  for i in 0..(ctype.size-1) do
    init[i] = read_expr(0)
    result_type('=', init[i].ctype, ctype.ptr)
    tok = read_token()
    if is_punct(tok, '}') && (i == ctype.size - 1)
      break
    end
    if !is_punct(tok, ',')
      error("comma expected, but got %s"%token_to_string(tok))
    end
    if i == ctype.size - 1
      tok = read_token()
      if !is_punct(tok, '}')
        error("'}' expected, but got %s"%token_to_string(tok))
      break;
      end
    end
  end
  return Ast::ast_array_init(ctype.size, init)
end

def read_decl_initializer(ctype)
  if ctype.type == CTYPE::ARRAY
    return read_decl_array_initializer(ctype)
  end
  return read_expr(0)
end

def read_decl
  ctype = get_ctype(read_token())
  while true
    tok = read_token()
    break unless is_punct(tok, '*')
    ctype = CTYPE::make_ptr_type(ctype)
  end
  error("Identifier expected, but got %s"%token_to_string(tok)) if tok.type != Token::IDENT
  varname = tok
  while true
    tok = read_token()
    if is_punct(tok, '[')
      size = read_expr(0)
      if size.type != Ast::LITERAL || size.ctype.type != CTYPE::INT
        error("Integer expected, but got %s"%ast_to_string(size))
      end
      expect(']')
      ctype = CTYPE::make_array_type(ctype, size.ival)
    else
      unget_token(tok)
      break
    end
  end
  var = Ast::ast_lvar(ctype, varname.sval)
  expect('=')
  init = read_decl_initializer(ctype)
  return Ast::ast_decl(var, init)
end

def read_decl_or_stmt
  tok = peek_token()
  return nil unless tok
  r = is_type_keyword(tok) ? read_decl() : read_expr(0)
  tok = read_token()
  error("Unterminated expression: %s"%token_to_string(tok)) unless is_punct(tok, ';')
  return r
end

def ctype_size(ctype)
  case ctype.type
  when CTYPE::CHAR ; return 1
  when CTYPE::INT  ; return 4
  when CTYPE::PTR  ; return 8
  when CTYPE::ARRAY; return ctype_size(ctype.ptr) * ctype.size
  else               error("internal error")
  end
end

def emit_gload(ctype, label, off)
  if ctype.type == CTYPE::ARRAY
    printf("lea %s(%%rip), %%rax\n\t", label)
    if (off)
      printf("add $%d, %%rax\n\t", ctype_size(ctype.ptr) * off)
    end
    return
  end
  size = ctype_size(ctype)
  case size
  when 1; reg = "al"; printf("mov $0, %%eax\n\t")
  when 4; reg = "eax"
  when 8; reg = "rax"
  else
    error("Unknown data size: %s: %d", ctype_to_string(ctype), size)
  end
  printf("mov %s(%%rip), %%%s\n\t", label, reg)
  if off > 0
    printf("add $%d, %%rax\n\t", off * size)
  end
  printf("mov (%%rax), %%%s\n\t", reg)
end

def emit_lload(var, off)
  if var.ctype.type == CTYPE::ARRAY
    printf("lea -%d(%%rbp), %%rax\n\t", var.loff)
    return
  end
  size = ctype_size(var.ctype)
  case size
  when 1
      printf("mov $0, %%eax\n\t")
      printf("mov -%d(%%rbp), %%al\n\t", var.loff)
  when 4
      printf("mov -%d(%%rbp), %%eax\n\t", var.loff)
  when 8
      printf("mov -%d(%%rbp), %%rax\n\t", var.loff)
  else
      error("Unknown data size: %s: %d", ast_to_string(var), size)
  end
  if off > 0
    printf("add $%d, %%rax\n\t", var.loff * size);
  end
end

def emit_gsave(var, off)
  assert(var.ctype.type != CTYPE::ARRAY)
  printf("push %%rbx\n\t")
  printf("mov %s(%%rip), %%rbx\n\t", var.glabel)
  size = ctype_size(var.ctype)
  case size
    when 1; reg = "al"
    when 4; reg = "eax"
    when 8; reg = "rax"
    else
      error("Unknown data size: %s: %d", ast_to_string(var), size)
  end
  printf("mov %s, %d(%%rbp)\n\t", reg, off * size)
  printf("pop %%rbx\n\t")
end

def emit_lsave(ctype, loff, off)
  size = ctype_size(ctype)
  case size
    when 1; reg = "al"
    when 4; reg = "eax"
    when 8; reg = "rax"
  end
  printf("mov %%%s, -%d(%%rbp)\n\t", reg, loff + off * size)
end

def emit_pointer_arith(op, left, right)
  assert(left.ctype.type == CTYPE::PTR)
  emit_expr(left)
  printf("push %%rax\n\t")
  emit_expr(right)
  size = ctype_size(left.ctype.ptr)
  if size > 1
    printf("imul $%d, %%rax\n\t", size)
  end
  printf("mov %%rax, %%rbx\n\t" +
         "pop %%rax\n\t" +
         "add %%rbx, %%rax\n\t")
end

def emit_assign(var, value)
  emit_expr(value)
  case var.type
  when Ast::LVAR; emit_lsave(var.ctype, var.loff, 0)
  when Ast::LREF; emit_lsave(var.lref.ctype, var.lref.loff, var.loff)
  when Ast::GVAR; emit_gsave(var, 0)
  when Ast::GREF; emit_gsave(var.gref, var.goff)
  else            error("internal error")
  end
end

def emit_binop(ast)
  if ast.type == '='
    emit_assign(ast.left, ast.right)
    return
  end
  if ast.ctype.type == CTYPE::PTR
    emit_pointer_arith(ast.type, ast.left, ast.right)
    return;
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
      printf("mov $%d, %%eax\n\t", ast.ival)
    when CTYPE::CHAR
      printf("mov $%d, %%rax\n\t", ast.c)
    else
      error("internal error")
    end
  when Ast::STRING
    printf("lea %s(%%rip), %%rax\n\t", ast.slabel)
  when Ast::LVAR
    emit_lload(ast, 0)
  when Ast::LREF
    assert(ast.lref.type == Ast::LVAR)
    emit_lload(ast.lref, ast.lrefoff)
  when Ast::GVAR
    emit_gload(ast.ctype, ast.glabel, 0)
  when Ast::GREF
    if ast.gref.type == Ast::STRING
      printf("lea %s(%%rip), %%rax\n\t", ast.gref.slabel)
    else
      assert(ast.gref.type == AST_GVAR)
      emit_gload(ast.gref.ctype, ast.gref.glabel, ast.goff)
    end
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
    if ast.decl_init.type == Ast::ARRAY_INIT
        for i in 0..(ast.decl_init.size - 1) do
          emit_expr(ast.decl_init.array_init[i])
          emit_lsave(ast.decl_var.ctype.ptr, ast.decl_var.loff, -i)
        end
    elsif ast.decl_var.ctype.type == CTYPE::ARRAY
        assert(ast.decl_init.type == Ast::STRING)
        i = 0
        for p in ast.decl_init.sval.split('') do
          printf("movb $%d, -%d(%%rbp)\n\t", p.ord, ast.decl_var.loff - i)
          i += 1
        end
        printf("movb $0, -%d(%%rbp)\n\t", ast.decl_var.loff - i)
    elsif ast.decl_init.type == Ast::STRING
        emit_gload(ast.decl_init.ctype, ast.decl_init.slabel, 0)
        emit_lsave(ast.decl_var.ctype, ast.decl_var.loff, 0)
    else
        emit_expr(ast.decl_init)
        emit_lsave(ast.decl_var.ctype, ast.decl_var.loff, 0)
    end
    return
  when Ast::ADDR
    assert(ast.operand.type == Ast::LVAR)
    printf("lea -%d(%%rbp), %%rax\n\t", ast.operand.loff)
  when Ast::DEREF
    assert(ast.operand.ctype.type == CTYPE::PTR)
    emit_expr(ast.operand)
    case ctype_size(ast.ctype)
    when 1; reg = "%bl"
    when 4; reg = "%ebx"
    when 8; reg = "%rbx"
    else    error("internal error")
    end
    printf("mov $0, %%ebx\n\t")
    printf("mov (%%rax), %s\n\t", reg)
    printf("mov %%rbx, %%rax\n\t")
  else
    emit_binop(ast)
  end
end

def ctype_to_string(ctype)
  case (ctype.type)
  when CTYPE::VOID; return "void"
  when CTYPE::INT ; return "int"
  when CTYPE::CHAR; return "char"
  when CTYPE::PTR ; return "%s*"%ctype_to_string(ctype.ptr)
  when CTYPE::ARRAY; return "%s[%d]"%[ctype_to_string(ctype.ptr), ctype.size]
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
    end
  when Ast::STRING
    buf << "\"%s\"" % ast.sval
  when Ast::LVAR
    buf << ast.lname
  when Ast::GVAR
    buf << ast.gname
  when Ast::LREF
    buf << "%s[%d]"%[ast_to_string(ast.lref), ast.lrefoff]
  when Ast::GREF
    buf << "%s[%d]"%[ast_to_string(ast.gref), ast.goff]
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
             ast.decl_var.lname,
             ast_to_string(ast.decl_init)]
  when Ast::ARRAY_INIT
   buf << "{"
   for i in 0..(ast.size - 1) do
     ast_to_string_int(ast.array_init[i], buf)
     if (i != ast.size - 1)
       buf << ","
     end
   end
   buf << "}"
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
  return if Ast::globals.empty?
  printf("\t.data\n")
  for p in Ast::globals do
    printf("%s:\n\t", p.slabel)
    printf(".string \"%s\"\n", p.sval)
  end
  printf("\t")
end

def ceil8(n)
  rem = n % 8
  return (rem == 0) ? n : n - rem + 8
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
    off = 0
    for ast in Ast::locals do
      off += ceil8(ctype_size(ast.ctype))
      ast.loff = off
    end
    emit_data_section()
    printf(".text\n\t" +
           ".global mymain\n" +
           "mymain:\n\t" +
           "push %%rbp\n\t" +
           "mov %%rsp, %%rbp\n\t")
    unless Ast::locals.empty?
      printf("sub $%d, %%rsp\n\t", off)
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
