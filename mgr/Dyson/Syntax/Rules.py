from pyparsing import *
from Utils import *

# ====================================================================================================
# Parser Configuration
#
# Parser Directive: !$parser or !$PARSER
#
# Adding a line !$parser <any text> to a source file will cause a single line of python
# code to be executed.  This is useful for debugging purposes and for changing the behavior
# of the parser for different styles of geometry files.  For example:
#
# !$PARSER setVerbose(1)                       # shows info on matching
# !$PARSER setVerbose(-1)                      # switches off warnings 
# !$PARSER setContinuation('f77')              # switches to f77 continuation style (char in col 6)
# !$PARSER setContinuation('mortran')          # switches back to mortran continuation style (trailing underscore (_),  comma (,) or mathematical operator (+-/*)
#
# Parser Hints
#
# "$PARSER Hint(Inline)" function(x) = b*b + 2.*b   # Declares an inline function
#

ExporterMortran = Keyword("Mortran")
ExporterAgML    = Keyword("AgML")
ExporterAgDL    = Keyword("AgDL")
ExporterAgROOT  = Keyword("AgROOT")
Exporter        = ExporterMortran | ExporterAgML | ExporterAgDL | ExporterAgROOT

InlineVar       = Word(alphas,alphanums+'_') + colon.suppress() + Word(alphas)

ParserAction  = CaselessLiteral('!$parser') + restOfLine ('action')
ParserMessage = CaselessLiteral('!$') + restOfLine('message')
ParserEmit    = CaselessLiteral('!$emit') + restOfLine('tag') 

ParserKeep    = dquote + Keyword("$Keep")   + lparen + Exporter('exporter')             + rparen + dquote + restOfLine('line')
ParserInline  = dquote + Keyword("$Inline") + lparen + delimitedList(InlineVar)('list') + rparen + dquote + restOfLine('line')

ParserSkip    = dquote + Keyword("$Skip") + dquote

verbose       = 0
warnings      = 0
continuation  = 'mortran'

# ======================================================================================== KEYWORDS ==

MIXTURE   = CaselessKeyword('MIXTURE')
MATERIAL  = CaselessKeyword('MATERIAL')
MEDIUM    = CaselessKeyword('MEDIUM')
COMPONENT = CaselessKeyword('COMPONENT')
SHAPE     = CaselessKeyword('SHAPE')
ATTRIBUTE = CaselessKeyword('ATTRIBUTE')
HITS      = CaselessKeyword('HITS')
CHECK     = CaselessKeyword('CHECK')
CREATE    = CaselessKeyword('CREATE')
POSITION  = CaselessKeyword('POSITION')
CREATEPOS = CREATE + CaselessKeyword('AND') + POSITION
MODULE    = CaselessKeyword('MODULE')
AUTHOR    = CaselessKeyword('AUTHOR')
CREATED   = CaselessKeyword('CREATED')
STRUCTURE = CaselessKeyword('STRUCTURE')

EXTERNAL  = CaselessKeyword('EXTERNAL')
DATA      = CaselessKeyword('DATA')

REAL      = CaselessKeyword('REAL')      + Optional ( Combine(star + number_int )) ('bytes') 
INTEGER   = CaselessKeyword('INTEGER')   + Optional ( Combine(star + number_int )) ('bytes') 
CHARACTER = CaselessKeyword('CHARACTER') + Optional ( Combine(star + number_int )) ('bytes') 
LOGICAL   = CaselessKeyword('LOGICAL')

TYPE      = INTEGER|REAL|CHARACTER

CALL      = CaselessKeyword('CALL')

DO        = CaselessKeyword('DO')
WHILE     = CaselessKeyword('DO')  + CaselessKeyword('WHILE')
FOR       = CaselessKeyword('FOR')

SUBROUTINE = CaselessKeyword('SUBROUTINE')
FUNCTION   = CaselessKeyword('FUNCTION')

IDENTIFIER = Word(alphas,alphanums+'_') ('identifier') 

GSTPAR     = CALL + CaselessKeyword('GSTPAR')
EMCUTS     = CaselessKeyword('SET') + CaselessKeyword('EMCUTS')

# Matches single-line CPP macros
CppMacro   = sharp + Keyword('define') + restOfLine('line')

# ====================================================================================================

AgINT  =   CaselessLiteral('int')
AgCHAR =   CaselessLiteral('char')
AgPRIN = ( CaselessKeyword('Prin0') |
           CaselessKeyword('Prin1') |
           CaselessKeyword('Prin2') |
           CaselessKeyword('Prin3') |
           CaselessKeyword('Prin4') |
           CaselessKeyword('Prin5') |
           CaselessKeyword('Prin6') |
           CaselessKeyword('Prin7') |
           CaselessKeyword('Prin8') |
           CaselessKeyword('Prin9') )

AgATTR = Combine(
    Word(alphas,alphanums+'_')    +
    Optional( OneOrMore(' ') ) +
    Literal('=')                  +
    Optional( OneOrMore(' ') ) +    
    Optional("'").suppress()      +
    Word(alphanums+'_()+-*/.{} ')    +
    Optional("'").suppress()     
    )

# ====================================================================================================

# Mortran style "comments"
ALLOWED = OneOrMore( CharsNotIn( "\"" ) )
AgMStyleComment = (
    dquote.suppress()                        +
    ALLOWED                                  +
    dquote.suppress()                        
    )

AgInlinedComment = (
    Optional(ALLOWED)    ('pre')            +
    AgMStyleComment      ('comment')        +
    Optional(ALLOWED)    ('post')
    )

# ====================================================================================================

AgModule    = (                                       # Matches MODULE NAME Documentation Line
    CaselessKeyword('MODULE')                  +      # --------------------------------------
    Word(alphas, alphanums)       ('name')     +      # name -- the name of the module
    restOfLine ('comment')                            # comment -- the comment line
    )

# ====================================================================================================

AgCreated   =  (
    CaselessKeyword('CREATED') +
    restOfLine('date')
    )

# ====================================================================================================

AgAuthor    =  (
    CaselessKeyword('AUTHOR' ) +
    OneOrMore( Word(alphanums+'<>@(),.') ) ('name')
    )

# ====================================================================================================

AgModuleEnd =        CaselessKeyword('END')

# ====================================================================================================

_cde = CaselessKeyword('CDE')
_inc = CaselessKeyword('INCLUDE')
AgCmzList   = (
    plus                                     +
    (_cde|_inc)                              +
    ','                                      +
    delimitedList( Word(alphanums) )('list') +
    '.'
    )

# ====================================================================================================

AgContent   = (
    CaselessKeyword('CONTENT')            +
    delimitedList(AgNAME) ('list')
    )

# ====================================================================================================

AgVar       =       (((AgINT|AgCHAR) + Word(alphanums+'()_')) | (Word(alphanums+'()_')) )

# LIMITATION: Structure's termination } must not appear on a new line...
#             ======== not allowed =========      ======== allowed ========
#             Structure ABCD { version, type      Structure ABCD { version, 
#             }                                                    type }
AgStructure =  (
    STRUCTURE                                          +
    AgNICK.setResultsName('name')                      +
    '{'                                                +
    delimitedList( AgVar ).setResultsName('variables') +
    Optional( White() ) +
    '}'
    )

# ====================================================================================================



__OLD__AgVarlist   = ( ( CaselessKeyword('Integer')|
                         CaselessKeyword('Real'   ) ).setResultsName('type') +
                       delimitedList( Word(alphanums+"()_") ).setResultsName('list')
                       )

TARGET = Word(alphanums+'():_')
ASSIGN = '/' + Word('0123456789.eE+-') + '/'

AgVarlist = ( TYPE                                           ('type' )            +
              delimitedList( Combine(TARGET + Optional(ASSIGN)) )     ('list' )
              )
             
    
    


# ====================================================================================================

AgParameter = (
    CaselessKeyword('Parameter') +
    lparen.suppress() +
    delimitedList(Combine((Word(alphanums+'_ ')+'='+Optional(' ')+(number_float|number_int)+Optional(' ')))) ('list') +
    rparen.suppress()
    )

#
# TODO: Automatic match of inline functions
#
# Inline functions cannot be easily differentiated from assignment statements.
# In order to do this, we would need to keep a list of all symbols which have
# been defined and whether they are an array or scalar, then throw to AgInline
# when we try to assign to a scalar.
#AgInline    = (
#    Word(alphas,alphanums+'_')                         ('name')             +
#    lparen.suppress()                                                       +
#    delimitedList( Combine( Word(alphanums+'_') ) )    ('arglist')          +
#    rparen.suppress() +'='                                                  +
#    delimitedList( Combine(Word(alphanums+'_()+-/*.'))) ('expr')
#    )

# ====================================================================================================

AgExternal  = ( EXTERNAL +
                delimitedList( Word(alphas,alphanums+'_') )  ('list')
                )

# ====================================================================================================

AgBlock     = (
    CaselessKeyword('BLOCK'   )                       +
    AgNAME                           ('name')         +
    Optional(
    OneOrMore(Word(printables))      ('comment')
    )
    )
AgBlockEnd  = CaselessKeyword('ENDBLOCK')

# ====================================================================================================

AgFill      = (
    CaselessKeyword('FILL' ) +
    AgNICK.setResultsName('name') +
    '!' +
    ( OneOrMore(  Word(printables)  )).setResultsName('comment')
    )

AgFillEnd   = CaselessKeyword('ENDFILL')


#AgArrayInit = (
#    Combine(    lbrack               +
#                delimitedList( (variable|number_float|number_int), delim=(comma|comma+newline) ) +
#                rbrack ) )

AgArrayInit = (
    Combine(     lbrack +
                 delimitedList( (variable|number_float|number_int), delim=(comma|comma+newline), combine=True ) +
                 rbrack
                 )
    )


AgArrayAttr  = Combine(
    Word(alphas,alphanums+'_')    +
    Optional( OneOrMore(' ') ) +
    Literal('=')                  +
    Optional( OneOrMore(' ') ) +    
    AgArrayInit
    )

AgAttrList   = OneOrMore( AgATTR + Optional(',').suppress() )


AgFillValue      = Word(alphanums+'_()*/+-. \'')

AgFillMatrix     = ( lbrack +
                     OneOrMore( delimitedList( AgFillValue ) + ';' ) +
                     rbrack )
AgFillArray      = ( lbrack +
                     delimitedList( AgFillValue ) +
                     rbrack )


AgFillAssignment = variable('name') + '=' + restOfLine('line')
                  

AgAssignment = ( variable                                     ('name')     +
                 Optional(space)                                           +
                 Optional( lparen + variable + rparen )       ('index')    +          # oversimplified
                 Optional(space)                                           +                 
                 '='                                                       +
                 Optional(space)                                           +                 
                 Optional( plus|minus )                                    +
                 Optional( OneOrMore(lparen) )                             +
                 (AgArrayInit|variable|number_float|number_int)            +
                 Optional( OneOrMore(rparen) )                             +
                 Optional( '!' ) + restOfLine
                 )

AgArrayAssignment = ( variable ('name')    + '=' +
                      Combine(lbrack    +   restOfLine ) ('line')
                      )

AgCharAssignment = ( variable ('name') + '=' + Literal("'") + Word(alphanums+' _') + Literal("'")  )

AgArrayElementAssignment = ( (variable+lparen+number_int+rparen) ('name') + '=' + restOfLine )


#AgCharAssignment = ( variable ('name') + '=' +
#                     "'" + Word( CharsNotIn("'") ) + "'" )

__OLD__AgAssignment = (
    variable.setResultsName('name')                +
    Literal('=')                                   +
    Optional( plus|minus )                         +
    Optional( OneOrMore(lparen) )                  +
    (AgArrayInit|variable|number_float|number_int) +
    Optional( Literal('!') +
              OneOrMore(Word(printables))
              )                                    +
    Optional( OneOrMore(rparen) )                  +
    Optional( '!' + OneOrMore(Word(printables)))
    )

assignment = (
    variable.setResultsName('name')                +
    Literal('=')                                   +
    Optional( plus|minus )                         +
    Optional( OneOrMore(lparen) )                  +
    (AgArrayInit|variable|number_float|number_int) +
    Optional( Literal('!') +
              OneOrMore(Word(printables))
              )                                    +
    Optional( OneOrMore(rparen) )                  +
    Optional( '!' + OneOrMore(Word(printables)))
    )


AgIncrement = (
    variable                                             ('name')            +
    (pluseq|minuseq|timeseq|divideeq)                    ('operator')        +
    Combine(OneOrMore(Word(alphanums+'_%+-*/.()')))      ('expr')            +
    Optional('!' + restOfLine)                           ('comment')
    )


AgFormat = (
    lparen                                                     +
    OneOrMore(Word(alphanums+'\'_@#$%^&*-+=_{}[]|:;<>,.?/~`'))   +
    rparen
    )


AgReplaceOpen  = (
    CaselessKeyword('REPLACE') +
    lbrace.suppress() +
    OneOrMore( Word( alphanums+'!@#$%^&*()_-+={}|\\:;\'\"<,>.?/' ) ) +
    rbrace.suppress() +
    CaselessKeyword('WITH') +
    lbrace )


AgReplaceClose = (
    Optional( OneOrMore( Word( alphanums+'!@#$%^&*()_-+={}|\\:;\'\"<,>.?/' ) ) ) +
    rbrace
    )


AgReplaceFull =  (
    CaselessKeyword('REPLACE') +
    lbrace.suppress() +
    OneOrMore( Word( alphanums+'!@#$%^&*()_-+={}|\\:;\'\"<,>.?/' ) )   ('match')   +
    rbrace.suppress() +
    CaselessKeyword('WITH') +
    lbrace.suppress() +
    OneOrMore( Word( alphanums+'!@#$%^&*()_-+={}|\\:;\'\"<,>.?/' ) )    ('with')    +
    rbrace.suppress() )
               

#AgGSTPar = ( GSTPAR + nestedExpr()     ('expr') )
#AgGSTPar = ( GSTPAR + restOfLine('expr') )
AgGSTPar = ( GSTPAR + lparen +
             Word(alphanums+'_')   ('imed')   + comma +
             "'" +
             Word(alphanums)       ('name')   +
             "'" + comma +
             Word(alphanums+'.Ee+- ')  ('value')  +
             rparen )

# Match command SET EMCUTS(struct_name)             
AgEmCUTS = EMCUTS + lparen.suppress() + Word(alphas) +rparen.suppress()


AgCall      = (
    CaselessKeyword('CALL')                   +
    Word(alphas, alphanums+'_')   ('routine') +
    nestedExpr()                     ('expr') 
    )


AgPrint = (
    CaselessLiteral('PRIN')                                    +
    Regex('[0-9]')                                ('level')    +
    Optional(
    delimitedList( Word(alphanums+'%*+-/_') )     ('list')    )
#    delimitedList( Word(CharsNotIn(',;')) )     ('list')    )
    )


AgPrintFmt = (
    CaselessLiteral('PRIN')                                                     +
    Regex('[0-9]')                                ('level')                     +
    Optional(delimitedList( Word(alphanums+'%*+-/_ ') )     ('list') )                      +
    semi.suppress()                                                             +
    lparen.suppress()                                                           +
    OneOrMore( (Word(alphanums+"',;:!@#$%^&*+=_-{}[]|\\?/<>,." )))   ('format') +
    rparen.suppress()
    ) 


# =
# = Matches DO WHILE loops (and not in general,
# = just really the one we have in the STAR
# = geometry codes)
#
AgWhileBegin = (
    WHILE('keyword') + Literal('(').suppress() +
    Combine(OneOrMore( Word(alphanums+'. ')))  ('expr')  +
    Literal(')').suppress()
    )
    
    
    


# Matches DO loops
AgLoopBegin = (
    DO                                               ('keyword')  +
    #    CaselessKeyword('DO') + 
    variable                                         ('variable') +
    Literal('=').suppress()                                       +
    OneOrMore( Word(alphanums+'%^&*()_-+|\\/' ))     ('lower')    +
    Literal(',').suppress()                                       +
    OneOrMore( Word(alphanums+'%^&*()_-+|\\/' ))     ('upper')    +
    Optional(    Literal(',').suppress()                                       +
                 OneOrMore( Word(alphanums+'%^&*()_-+|\\/' ))     ('stride')   
                 ))








AgLoopEnd   = (
    CaselessKeyword('ENDDO')  |  
    CaselessLiteral('END DO') 
    )


AgIfBegin = (
    CaselessKeyword('IF')             +
    nestedExpr()             ('expr') +
    CaselessKeyword('THEN')
    )
    

AgIfEnd     = (
    CaselessKeyword('ENDIF') | (CaselessKeyword('END') + CaselessKeyword('IF'))
    )


AgElseIf    = (
    (CaselessKeyword('ELSEIF') | (CaselessKeyword('ELSE')+CaselessKeyword('IF'))) +
    nestedExpr()         ('expr') +
    CaselessKeyword('THEN')
    )


AgElse      = CaselessKeyword('ELSE')


AgIfEnd     = (
    CaselessKeyword('ENDIF') | (CaselessKeyword('END')+CaselessKeyword('IF') )
    )


AgIfAssign  = (
    CaselessKeyword('IF')           + 
    nestedExpr()          ('expr') +
    restOfLine            ('assign')
    )


AgUse       = (
    CaselessKeyword('USE')                +
    AgNICK                   ('struct')   +
    Optional(AgATTR)         ('select')
    )

# LIMITATION: Only value-less returns (i.e. subroutines, no functions yet)
AgReturn    = (
    CaselessKeyword('RETURN') 
    )


AgData      = (
    DATA                                                    +
    delimitedList(variable)                   ('variables') +
    Literal('/').suppress()                                 +
#    delimitedList( Optional(plus|minus) +  number_float|number_int)      ('values')  +
    delimitedList( Word(alphanums+'+-.eE')  )      ('values')  +
    Literal('/').suppress()
    )

AgCreate    = (
    CREATE         ('keyword')          +
    AgNICK         ('block')            +
    restOfLine     ('line')
    )

AgPosition = (                                                # Limitation:  Cannot interleave
    CaselessKeyword('POSITION')                           +   # key=value pairs and 'in MTHR'
    AgNICK                                ('block')       +   # flag.  So:
    Optional( CaselessKeyword('IN')                       +
              AgNICK                      ('mother')
              )                                           +
    restOfLine                            ('line')    
    )

AgCreatePos = (
    CaselessKeyword('CREATE') +
    CaselessKeyword('and')    +
    CaselessKeyword('POSITION')                           +   # key=value pairs and 'in MTHR'
    AgNICK                                ('block')       +   # flag.  So:
    Optional( CaselessKeyword('IN') +
              AgNICK                      ('mother')
              ) +
    restOfLine                            ('line')
    )


AgMixture   = (    CaselessKeyword('MIXTURE')           +
                   variable                    ('name') +
                   restOfLine                  ('list') )

AgComponent = (
    CaselessKeyword  ('COMPONENT')  +
    variable         ('name')       +
    restOfLine       ('attributes')
    )

#AgMaterial  = CaselessKeyword('MATERIAL')  + variable.setResultsName('name') + Optional( AgAttrList.setResultsName('list') )

AgMaterial = ( MATERIAL           ('keyword')              +
               variable           ('name')                 +
               restOfLine         ('attributes')           )


AgMedium    = CaselessKeyword('MEDIUM'  )  + variable.setResultsName('name') + Optional( AgAttrList.setResultsName('list') )

AgAttribute = (
    CaselessKeyword('ATTRIBUTE') +
    AgNICK ('for')               +
    restOfLine ('line' )
    )

AgImplicit  = CaselessKeyword('IMPLICIT')  + CaselessKeyword('NONE')

_shapes      = ( CaselessKeyword('BOX')|CaselessKeyword('DIVISION')|AgNICK )

AgShape     = (
    CaselessKeyword('SHAPE')                  +
    _shapes.setResultsName("type")            +
    restOfLine ('line' )
    #  Optional( AgAttrList ) ('list')
    )
# ====================================================================================================

AgCheck     = ( CHECK + restOfLine ('expr') )

AgHits      = ( CaselessKeyword('HITS')                                +
                AgNICK                                ('name')         +
##                Optional( Combine(CaselessKeyword('TYPE') + '=' + Word(alphanums+':')) ) ('type') +
                restOfLine                            ('definition')
#               OneOrMore( Word(alphanums+':(),+-.' ) )   ('definition')
                )

AgWhiteSpace = White() + LineEnd()

AgComment = (( ( ccomment|star|ecomment ) ('type') + 
               OneOrMore( Word( printables ))    ('comment')
               ) |
             LineStart() + (ccomment|ecomment|star) )('type')
             
             
AgSubroutine =                  SUBROUTINE + IDENTIFIER + lparen.suppress() +Optional(delimitedList(IDENTIFIER,','))+ rparen.suppress()
AgFunction   = Optional(TYPE) + FUNCTION   + IDENTIFIER + lparen.suppress() +Optional(delimitedList(IDENTIFIER,','))+ rparen.suppress() 

RULES = { 'ParserAction'  : ParserAction,         # Parser rules
          'ParserMessage' : ParserMessage,
          'ParserEmit'    : ParserEmit, 
                    
          'ParserSkip'    : ParserSkip,
          'ParserInline'  : ParserInline,

          'CppMacro'      : CppMacro,             # c-preprocessor maros
          
          'InlinedComment': AgInlinedComment,
          'Comment'       : AgComment,
          'Structure'     : AgStructure,
          'Fill'          : AgFill,
          'FillEnd'       : AgFillEnd,
          'Content'       : AgContent,
          'Varlist'       : AgVarlist,
          'Parameter'     : AgParameter,
          'LoopBegin'     : AgLoopBegin,
          'WhileBegin'    : AgWhileBegin,
          'LoopEnd'       : AgLoopEnd,
          'IfBegin'       : AgIfBegin,
          'IfEnd'         : AgIfEnd,
          'ElseIf'        : AgElseIf,
          'Else'          : AgElse,
          'Return'        : AgReturn,
          'Shape'         : AgShape,
          'Data'          : AgData,
          'Create'        : AgCreate,
          'Position'      : AgPosition,
          'CreatePos'     : AgCreatePos,
          'Material'      : AgMaterial,
          'Medium'        : AgMedium,
          'Mixture'       : AgMixture,
          'Component'     : AgComponent,
          'Hits'          : AgHits,
          'Attribute'     : AgAttribute,
          'Use'           : AgUse,
          'Print'         : AgPrint,          
          'CmzList'       : AgCmzList,
          'Module'        : AgModule,
          'Created'       : AgCreated,
          'Author'        : AgAuthor,
          'ModuleEnd'     : AgModuleEnd,
          'IfAssign'      : AgIfAssign,
          'Assignment'    : AgAssignment,
          'ArrayAssign'   : AgArrayAssignment,
          'CharAssign'    : AgCharAssignment,
          'ElementAssign' : AgArrayElementAssignment,
          'Increment'     : AgIncrement,
          'Format'        : AgFormat,
          'Replace'       : AgReplaceFull,
          'Block'         : AgBlock,          
          'BlockEnd'      : AgBlockEnd,
          'GSTPar'        : AgGSTPar,
          'EmCUTS'        : AgEmCUTS,
          'Call'          : AgCall,
          'ImplicitNone'  : AgImplicit,
          'Subroutine'    : AgSubroutine,
          'Check'         : AgCheck,
          'External'      : AgExternal,          
          'WhiteSpace'    : AgWhiteSpace
          }


ORDER = [ 'ParserAction',      # Prioritize parser rules
          'ParserEmit',
          'ParserInline',
          'ParserSkip',

          'ParserMessage',
          
          'CppMacro',          # Next look for CPP macros          
          'InlinedComment',    # Followed by comments
          'Comment'   ,          
          'Structure' ,        # And finally AgSTAR syntax
          'Fill'      , 
          'FillEnd'   , 
          'Content'   , 
          'Varlist'   , 
          'Parameter' , 
          'Print'     , 
          'LoopBegin' ,
          'WhileBegin',
          'LoopEnd'   , 
          'IfBegin'   , 
          'IfEnd'     , 
          'ElseIf'    , 
          'Else'      ,
          'IfAssign'  ,
          'Return'    , 
          'Shape'     , 
          'Data'      , 
          'Block'     , 
          'BlockEnd'  , 
          'Create'    , 
          'Position'  , 
          'CreatePos' , 
          'Material'  , 
          'Medium'    , 
          'Mixture'   , 
          'Component' , 
          'Hits'      , 
          'Attribute' , 
          'Use'       , 
          'CmzList'   , 
          'Module'    , 
          'Created'   , 
          'Author'    , 
          'Assignment',
          'ArrayAssign',
          'CharAssign',
          'ElementAssign',
          'Increment' , 
          'Format'    , 
          'Replace'   , 
          'GSTPar',
          'EmCUTS',
          'Call'         , 
          'ImplicitNone' , 
          'WhiteSpace'  ,
          'Check',
          'Subroutine',
          'External'  ,   
          'ModuleEnd'           
          ]

# Very limited form of replace macros matching
def makeRuleFromReplace( replace, With='' ):
    keywords = replace.split('#')
    RULE  = CaselessKeyword( keywords[0].strip('()') )
    if ( len(keywords)>0 ):
        RULE += Optional( lparen )
        RULE += Word(alphanums+'_')
        RULE += Optional( rparen )
    return RULE

        
def addRule( name, rule, action=None ):
    ORDER.append(name)
    RULES[name]=rule
    if ( action != None ):
        RULES[name].setParseAction( action )

last_match  = None
last_input  = ''

def tryRules(line):

    global last_match, last_input, verbose, warnings
    result     = None
    last_input = line
    last_key   = 'None'

    for slot,key in enumerate(ORDER):
        rule = RULES[key]
        try:                        
            result = rule.parseString(line)
            last_key = key
            break 
        except ParseException:
            pass

    if ( verbose >= 1 ):
        print ""
        print "[RULES]: -------------------------------------------------------------------"
        print "[RULES]: line:    "+line
        print "[RULES]: matched: "+last_key+" at slot %03i"%slot
    if ( verbose >= 2 ):
        print "[RULES]: result:  "+str(result)
        print "[RULES]: last:    "+str(last_match)                
    if ( verbose >= 3 ):
        if ( result != None ):
            print "[RULES]: result:  "+str(result.asDict())
        else:
            print "[RULES]: result:  "+str(result)

    if ( verbose >= 1 ):
        print "[RULES]: -------------------------------------------------------------------"

    if ( result == None and line.strip() != '' and verbose > -1 ):
        warnings+=1
        AgComment.parseString( "!$ [WARNING]: unmatched line"  )
        AgComment.parseString( "!$ [WARNING]: %s"%line         )
            
    last_match = result
    
    return


# Utilities for delimiting statements

def reverse( string ):
    buffer = []
    for c in string:
        buffer.append(c)
    newstr = ""
    while ( len(buffer) ):
        newstr += buffer.pop()
    return newstr

def lastChar( fline, mychar=';' ):
    rline = reverse(fline)
    match = Literal(mychar) + ZeroOrMore( Word( printables ) )
    try:
        result=match.parseString(rline)
        #        print '[T]' + rline        
        return True
    except ParseException:
        #        print '[F]' + rline                
        return False

def isComment( fline ):
    try:
        result=RULES['Comment'].parseString(fline)
        return True
    except ParseException:
        return False

def isPrint( fline ):
    try:
        result=RULES['Print'].parseString(fline)
        return True
    except ParseException:
        return False

def isWhite( fline ):
    try:
        result=RULES['WhiteSpace'].parseString(fline)
        return True
    except ParseException:
        return False

# ----------------------------------------------------------------------------------------------------

def delimitStatementFortran( fline, nline=None ):
    """
    Looks for any character in the 6th column (5 counting from 0)
    to determine whether this line and/or the next line are
    continuation lines.
    """
    cf = fline[5]
    cn = nline[5]


    return fline
    

def delimitStatement( fline, nline=None ):
    """
    Looks for a trailing , _ or ; on a line of input.  The line will be treated
    as a split statement if it ends with , or _.  Otherwise, it will be treated
    as one complete Mortran statement.

    The exception is for comment lines where a leading *, c, C or ! is detected
    """

    # Pass whitespace through untouched
    try:
        result = RULES['WhiteSpace'].parseString( fline )
        return fline
    except ParseException:
        pass

    # Pass comment lines through untouched
    try:
        result = RULES['Comment'].parseString( fline )
        return fline
    except ParseException:
        pass
    
    rline  = reverse( fline )

    ops = (plus|minus) # times and divide not likely legal continuation operators
    
    match  = (
        (comma|score|semi|ops)   ('char') +
        ZeroOrMore( Word( printables ) )
        )
    # Pass lines with a trailing continuation character through
    # mostly untouched.  Strip trailing '_' from output.
    try:
        result = match.parseString(rline)
        char = result.get('char')
        if ( char == '_' ):
            fline=fline.strip() # strip leading/trailing white space
            fline=fline[:len(fline)-1]
        return fline

    # All other lines are complete statementes and get a ';' appended
    # to them.  These will be stripped out later for output.
    except ParseException:
        return fline.strip('\n')+' ;\n'



def setVerbose(lvl):
    global verbose
    verbose = lvl
def numberOfWarnings():
    global warnings
    return warnings
def setContinuation(style):
    global continuation
    style=style.lower()
    if ( style != 'mortran' and style != 'fortran' ):
        print "!$ [RULES]: WARNING: %s is not a recognized continuation style"%style
    continuation=style



if __name__ == '__main__':
    #              1         2         3         4         5         6         7         8         9        10        11        12
    #     123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
    TEST="Structure TROV { version, array(10), matrix(11,12) }";
    result = AgStructure.parseString(TEST)
    print result

    
#   TEST="      Subroutine ecal_get_strip( section, cut, istrip, xcenter, ycenter, length )"
#   result = AgSubroutine.parseString(TEST)
#    print result
#    
#    TEST="      real function joy(j,o,y)"
#    result = AgFunction.parseString(TEST)
#    print result

    
    
#    TEST="  Tanf(x) =  tan(2*atan(exp(-etax)))"
#    result = AgInline.parseString(TEST)
#    result = AgAssignment.parseString(TEST)
#    print result
