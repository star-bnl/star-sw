from pyparsing import *

"""
Several pyparsing rules matching AgSTAR keywords and tokens
"""

# Some basic constants
_digits = "1234567890"
_upperc = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
_lowerc = _upperc.lower()

_period = "."
_comma  = ","
_dash   = "-"
_colon  = ":"
_semi   = ";"
_slash  = "/"
_bslash = "\\"
_equal  = "="
_bang   = "!"
_tilda  = "~"
_sharp  = "#"
_space  = " "
_score  = "_"
_newln  = "\n"
_star   = "*"
_plus   = "+"
_minus  = "-"
_dollar = "$"
_dquote = '"'
_squote = "'"

_lparen = "("
_rparen = ")"
_lbrack = "{"
_rbrack = "}"
_lbrace = "["
_rbrace = "]"


# Punctuation
period  = Literal( _period )
decimal = Literal( _period )
comma   = Literal( _comma  )
bang    = Literal( _bang   ) # i.e. !
sharp   = Literal( _sharp  )
colon   = Literal( _colon  )
semi    = Literal( _semi   )
equal   = Literal( _equal  )
space   = Literal( _space  )
score   = Literal( _score  )
newline = Literal( _newln  )
star    = Literal( _star   )
plus    = Literal( _plus   )
minus   = Literal( _minus  )
times   = Literal( _star   )
divide  = Literal( _slash  )
dollar  = Literal( _dollar )

dquote  = Literal( _dquote )
squote  = Literal( _squote )

lparen  = Literal( _lparen )
rparen  = Literal( _rparen )
lbrack  = Literal( _lbrack )
rbrack  = Literal( _rbrack )
lbrace  = Literal( _lbrace )
rbrace  = Literal( _rbrace )

pluseq    = Literal( "+=" )
timeseq   = Literal( "-=" )
minuseq   = Literal( "*=" )
divideeq  = Literal( "/=" )

newline = '\n'

# Expos logs sqrt etc...
SQRT  = CaselessKeyword("sqrt")
EXPO  = CaselessKeyword("exp")
LOG   = CaselessKeyword("log") | CaselessKeyword("ln")
MOD   = CaselessKeyword("mod")

# Trig functions
SIN  = CaselessKeyword("sin")
COS  = CaselessKeyword("cos")
TAN  = CaselessKeyword("tan")
ASIN = CaselessKeyword("asin")
ACOS = CaselessKeyword("acos")
ATAN = CaselessKeyword("atan")

# Hyperbolic trig functions
SINH  = CaselessKeyword("sinh")
COSH  = CaselessKeyword("cosh")
TANH  = CaselessKeyword("tanh")
ASINH = CaselessKeyword("asinh")
ACOSH = CaselessKeyword("acosh")
ATANH = CaselessKeyword("atanh")

funcs = ( SQRT  | EXPO  | LOG   | MOD |
          SIN   | COS   | TAN   |
          ASIN  | ACOS  | ATAN  |
          SINH  | COSH  | TANH  |
          ASINH | ACOSH | ATANH )

# Mathematical Operators
opAdd = plus         | minus
opMul = times        | divide
opPow = Literal("^") | Literal( "**" )

# Logical operators
logEq    = Literal("==") | CaselessLiteral(".eq.")
logGt    = Literal(">")  | CaselessLiteral(".gt.")
logLt    = Literal("<")  | CaselessLiteral(".lt.")
logOr    = Literal("|")  | CaselessLiteral(".or.")
logNeq   = Literal("!=") | Literal("<>") | CaselessLiteral(".neq.")
logNot   = CaselessLiteral(".not.")
logAnd   = Literal("&") | CaselessLiteral(".and.")

# Logical constants
logTrue  = CaselessLiteral(".true.")
logFalse = CaselessLiteral(".false.")

# Comments:  note that comment matching must wait until the end,
#            so that create, cos, etc... can be present in free
#            form
ccomment     = LineStart() + ( CaselessLiteral('c') + NotAny( CaselessLiteral("reate")|CaselessLiteral("ontent") ) | star)
ecomment     = bang

comment_line  = ( ccomment|ecomment ) + OneOrMore( Word( printables ) )


# Some basic data types
number       = Word( _digits )
number_int   = Optional(plus|minus) + Combine( number + Optional( Word("eE") + Word("+-") + number ) )
number_float = Optional(plus|minus) + Combine( Optional(number) + decimal + Optional( number ) + Optional( Word("eE") + Word("+-") + number ) )

number_list  = Combine( (number_float|number_int) + OneOrMore( comma + (number_float|number_int) ) )
number_array = ( lbrack +
                 number_list +
                 rbrack )

# Variable names
variable = Word( alphas, alphanums + "_" )
variable_list = Combine( variable + OneOrMore( comma + variable ) )

array = Word( alphas, alphanums +"_" ) + '(' + number_int + ')'

#+ delimitedList( (number_float|number_int), delim=',', combine=False ) + Literal('}')

# AgSTAR tokens
AgNAME   = Word( alphas, exact=4 )                                     # 4 character name
AgSTRU   = Combine( AgNAME + Word( srange("[A-Za-z0-9_]") ) )
AgNICK  = Word( alphas, alphanums, exact=4 )

#AgCONT   = LineEnd( Literal( score | comma ) )                        # line continuation
#AgATTR   = Word(alphas, alphanums) + equal + ( number_float | number_int | number_array )

identifier = ( variable | AgSTRU )
identifier_list = identifier + OneOrMore( comma + identifier )



def matchRule( line, rule ):
    try:
        rule.parseString(line)
        return True
    except ParseException:
        return False
    


if __name__ == "__main__":

    print ""
    print "Floating point values:"
    print "======================"
    for fp in ["1.234", "1234.", "1.234E+56","3.14159265359"]:
        print fp + " --> " + str( number_float.parseString( fp ) )
    print ""
    print "Integral values:"
    print "================"
    for iv in ["1234", "1234E+5", "1234E-5"]:
        print iv + " --> " + str( number_int.parseString( iv ) )        
    print ""
    print "Lists of numbers:"
    print "================="
    for l in ["1,2,3,4","1.,2.,3.,4.","1,2.,3,4.","1.,2,3.,4"]:
        print l + " --> " + str( number_list.parseString(l) )
    print ""
    print "Comments:"
    print "========="
    for comment in ["c is a comment line", "C is a comment line", "* is a comment line", "! is a comment line","    ! this is another ! comment","c.reate and position"]:
        print comment + " --> " + str( comment_line.parseString( comment ) )
    print ""
    print "Variables:"
    print "=========="
    for var in ["abc", "ABC", "abc_test", "ABC_tEsT", "A13_x743__ttuU___", "z_____________" ]:
        print var + " --> " + str( variable.parseString( var ) )
    print ""
    print "AgSTAR Names (i.e. blocks, structures, etc...):"
    print "==============================================="
    for name in ("BBCM", "BBCG", "HEXG"):
        print name + " --> " + str( AgNAME.parseString(name) )
    for var in ("BBCM_version", "BBCM_type", "bbcg_irad" ):
        print var  + " --> " + str( AgSTRU.parseString(var) )
