
$ungotten = nil

module Token
  IDENT =0
  PUNCT =1
  INT   =2
  CHAR  =3
  STRING=4

  Ident = Struct.new :type, :sval
  Punct = Struct.new :type, :punct
  Int   = Struct.new :type, :ival
  Char  = Struct.new :type, :c
  String= Struct.new :type, :sval

  def make_ident(s)
    Ident.new IDENT, s
  end
  def make_strtok(s)
    String.new STRING, s
  end
  def make_punct(punct)
    Punct.new PUNCT, punct
  end
  def make_int(ival)
    Int.new INT, ival
  end
  def make_char(c)
    Char.new CHAR, c
  end
  module_function :make_ident, :make_strtok, :make_punct, :make_int, :make_char
end

def skip_space
  while !STDIN.eof
    c = STDIN.getc
    next if c.strip == ""
    STDIN.ungetc(c)
    return;
  end
end

def read_number(n)
  while c = STDIN.getc
    unless c.numeric?
      STDIN.ungetc(c)
      return Token::make_int(n)
    end
    n = n*10 + c.to_i
  end
end

def read_char
  error("Unterminated string") if STDIN.eof
  c = STDIN.getc
  if c == '\\'
    error("Unterminated string") if STDIN.eof
    c = STDIN.getc
  end
  error("Unterminated string") if STDIN.eof
  c2 = STDIN.getc
  error("Malformed char constant") unless c2 == '\''
  return Token::make_char(c.ord)
end

def read_string
  s = String.new
  while true
    error("Unterminated string") if STDIN.eof
    c = STDIN.getc
    break if c == '"'
    if c == '\\'
      error("Unterminated string") if STDIN.eof
      c = STDIN.getc
    end
    s << c
  end
  return Token::make_strtok(s)
end

def read_ident(c)
  s = String.new
  s << c
  while true
    c2 = STDIN.getc
    if c2.alnum?
      s << c2
    else
      STDIN.ungetc(c2)
      return Token::make_ident(s)
    end
  end
end

def read_token_init
  skip_space()
  return nil if STDIN.eof
  c = STDIN.getc
  case c
  when '0','1','2','3','4','5','6','7','8','9'
    return read_number(c.to_i)
  when '"'
    return read_string()
  when '\''
    return read_char()
  when 'a','b','c','d','e','f','g','h','i','j','k','l','m','n',
       'o','p','q','r','s','t','u','v','w','x','y','z','A','B',
       'C','D','E','F','G','H','I','J','K','L','M','M','N','O',
       'P','Q','R','S','T','U','V','W','X','Y','Z','_'
    return read_ident(c)
  when '/','=','*','+','-','(',')',',',';'
    return Token::make_punct(c)
  else
    error("Unexpected character: '%c'"%c);
  end
end

def token_to_string(tok)
  case tok.type
  when Token::IDENT ; return tok.sval
  when Token::PUNCT ; return tok.punct.to_s
  when Token::CHAR  ; return tok.c.to_s
  when Token::INT   ; return tok.ival.to_s
  when Token::STRING; return tok.sval
  else
    error("internal error: unknown token type: %d"%tok.type)
  end
end

def is_punct(tok, c)
  error("Token is null") unless tok
  return tok.type == Token::PUNCT && tok.punct == c
end

def unget_token(tok)
  error("Push back buffer is already full") if $ungotten
  $ungotten = tok
end

def read_token
  if $ungotten
    tok = $ungotten
    $ungotten = nil
  else
    tok = read_token_init()
  end
  return tok
end
