stmts
  = s_expr*

s_expr
  = ws* '(' list:expr* ')'  ws* {
    return list;
  }

s_expr_prefix
  = "'" / '#'

value_expr
  = num
  / id
  / str

id
  = $([a-zA-Z0-9-+*/%=^.'#$]+)

num
  = num:$([0-9]+('.' [0-9]+)?) !id {
    return Number(num);
  }
  / num:$('.' [0-9]+) {
    return Number(num);
  }

str
  = "'" str:$([^'\n]*) "'" {
    return ['str', str];
  }
  / '"' str:$([^"\n]*) '"' {
    return ['str', str];
  }
  / '`' str:$([^`\n]*) '`' {
    return ['str', str];
  }

expr
  = ws* e:_expr ws* {
    return e;
  }

_expr
  = s_expr
  / value_expr

ws = '\r\n' / [\r\n\t ]
