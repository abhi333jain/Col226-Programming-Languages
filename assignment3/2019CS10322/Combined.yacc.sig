signature Check_TOKENS =
sig
type ('a,'b) token
type svalue
val BOOL: (string) *  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
val ARROW: (string) *  'a * 'a -> (svalue,'a) token
val FUN: (string) *  'a * 'a -> (svalue,'a) token
val IMP: (string) *  'a * 'a -> (svalue,'a) token
val COLON: (string) *  'a * 'a -> (svalue,'a) token
val FN: (string) *  'a * 'a -> (svalue,'a) token
val EQ: (string) *  'a * 'a -> (svalue,'a) token
val END: (string) *  'a * 'a -> (svalue,'a) token
val IN: (string) *  'a * 'a -> (svalue,'a) token
val LET: (string) *  'a * 'a -> (svalue,'a) token
val GREATERTHAN: (string) *  'a * 'a -> (svalue,'a) token
val LESSTHAN: (string) *  'a * 'a -> (svalue,'a) token
val NEGATE: (string) *  'a * 'a -> (svalue,'a) token
val TIMES: (string) *  'a * 'a -> (svalue,'a) token
val MINUS: (string) *  'a * 'a -> (svalue,'a) token
val PLUS: (string) *  'a * 'a -> (svalue,'a) token
val RPAREN: (string) *  'a * 'a -> (svalue,'a) token
val LPAREN: (string) *  'a * 'a -> (svalue,'a) token
val FI: (string) *  'a * 'a -> (svalue,'a) token
val ELSE: (string) *  'a * 'a -> (svalue,'a) token
val THEN: (string) *  'a * 'a -> (svalue,'a) token
val IF: (string) *  'a * 'a -> (svalue,'a) token
val IMPLIES: (string) *  'a * 'a -> (svalue,'a) token
val EQUALS: (string) *  'a * 'a -> (svalue,'a) token
val XOR: (string) *  'a * 'a -> (svalue,'a) token
val OR: (string) *  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val AND: (string) *  'a * 'a -> (svalue,'a) token
val NOT: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val CONST: (bool) *  'a * 'a -> (svalue,'a) token
val TERM: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Check_LRVALS=
sig
structure Tokens : Check_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
