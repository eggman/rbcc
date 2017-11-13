
def error(str)
  raise RuntimeError, str
end
  
def warn(str)
  raise RuntimeError, str
end
  
def assert(expr)
  raise RuntimeError, expr unless expr
end
