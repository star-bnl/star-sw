#!/usr/bin/env python

from pyparsing import *
import sys
sys.path.append(".")
sys.path.append("$STAR")
sys.dont_write_bytecode = True


#from Rules     import *
#from Utils     import *
from Dyson.Syntax.Rules import *
from Dyson.Syntax.Utils import *

__fill_term__ = 'EndFill'

from optparse import OptionParser
import pprint

#from SyntaxHandler import SyntaxHandler
#from SyntaxHandler import setExportSyntax

from Dyson.Syntax.SyntaxHandler import SyntaxHandler
from Dyson.Syntax.SyntaxHandler import setExportSyntax

syntax = None #SyntaxHandler('Mortran')
syntax_type = ''

PPrint = pprint.PrettyPrinter( indent=4 )
out    = PPrint.pprint

geomFile = None # The geometry file being parsed

buffer0  = ""   # Buffer of lines read in from the geometry as is
buffer1  = ""   # Buffer with ; terminated lines

geomLines = []
geomState = []

print_line = False

REPLACE   = {} # List of relpace macros pattern : result

_current_line = ""

def readFile( name ):
    """
    Opens the file and reads the file into two working buffers:
    + buffer0 -- the file as written
    + buffer1 -- the file with a ';' at the end of each complete statement.
                 Here, a statement will include any '!' comment present on
                 the line.
    """
    global buffer0, buffer1
    geomFile = open( name )
    for line in geomFile:
        buffer0 += line
        buffer1 += delimitStatement( line )

def chopLines( buffer, delim='\n' ):
    """
    Breaks the specified buffer into lines at every delimiter.  Returns a list
    of those lines.
    """
    list = []
    for line in buffer.split( delim ):
        list.append( line )

    return list
    

def groupStats( lines, delim=';' ):
    """
    Groups lines toghether which form complete statements, terminated by the
    specified delimiter.  Returns a list of statements.
    """
    statements = []
    index=0
    limit=len(lines)
    while ( index < len(lines) ):
        # Get the current line and advance the index
        if index==limit: break        
        line     = lines[index]        
        index   += 1
        # Start a new complete statement
        state    = line
        # Accumulate lines into this statement until
        # we encounter a trailing ';'

        #print '[>]' + line

        # A ';' terminates the line.  A comment line is
        # complete with or without the ';'
        #go = ( not lastChar( line, ';' ) and not isComment(line ) )
        go  = not lastChar(line,';')
        go &= not isComment(line)
        go &= not (len( line.strip(' ') )==0)
        isprin = isPrint(line)
        go |=     isPrint(line)
        
        while ( go ):
            if index==limit: break            
            line   = lines[index]
            #print '[+]' + line
            state+=line
            index += 1
            go  = not lastChar(line,';')
            go &= not isComment(line)
            go &= not (len( line.strip(' ') )==0)            
                   
        statements.append( state )

    # And return the list of ; delimited statements
    return statements

def groupReplace( lines ):
    """
    Loops over the input statements and looks for REPLACE statements
    which span multiple lines.  It will merge these into single statements,
    then return the modified list.
    """
    statements = []
    startBlock = AgReplaceOpen + Literal(';')
    endBlock   = Literal(']')  + Literal(';') 

    inBlock   = False
    index     = 0
    statement = ''
    for line in lines:
                
        if ( matchRule( line, startBlock ) ):
            inBlock = True

        if ( not inBlock ):
            statements.append( line )  # append line to list of statements
        else:
            #line = line.replace(';','\n')
            statement += line          # append line to current statement
            statement += '\n'
        
        if ( matchRule( line, endBlock ) ):
            inBlock = False
            #line = line.replace(';','\n')            
            #statement += line
            statements.append( statement )
            statement = ''

    return statements


def groupMatrix( lines ):
    """
    Loops over the input list of lines and merges back multi-dimensional
    arrays.  These arrays take the form

    array = { 1., 2., 3., 4.;
              5., 6., 7., 8.;
              ... }

    So... we need to find ; terminated lines which occur w/in an unclosed
    bracket.  Then combine until we find the bracket
    
    """
    buffer  = range(0,len(lines))  # there has to be a better way to do this
    for i in range(0,len(lines)):
        buffer[i]=""

    i=0;    j=0
    open_brace = False
    while ( i < len(lines) ):
        line = lines[i]
        for c in line:
            if ( c=='{' ): open_brace = True
            if ( c=='}' ): open_brace = False
            
        #print "%04i | %s"%( j, line )
        buffer[j]+= line.strip('\n')
        i+=1
        if ( not open_brace ): j+=1

    return buffer

def endOfFill( line ):
    """
    Returns true if the passed line is at the end of a FILL ... ENDFILL
    block, false otherwise.  Behavior depends on the global __FILL_TERM__
    variable.  If __FILL_TERM__ is 'NONE', then a whitespace line terminates
    the fill block.  Otherwise it will look for a match to AgfillEnd.
    """
    if __fill_term__ == 'NONE':
        if ( len(line.strip(' \t')) == 0 ): return True
    else:
        try:
            AgFillEnd.parseString(line)
            return True
        except ParseException:
            return False

#
# Some global state variables indicating where we are inside of a module
#
__in_module__ = False
__in_fill__   = False
__in_block__  = False
__finished__  = False

def OnAction( string, location, tokens ):
    """
    Handles special parser actions inserted by the user
    into the text being parsed.  These actions are denoted
    by the !$PARSER directive and must appear on a single
    line, alone.
    """
    AgComment.parseString( string )       # output stream as a comment
    action = tokens.get('action')
    action = action.lstrip()  # strip leftmost whitespace because it works that way...

    ISEXIT=Keyword('sys')+period+Keyword('exit') + restOfLine

    try:                                   # detect exit at this point and
        ISEXIT.parseString( action )             # properly end documents
        syntax.endElement('Document')
    except ParseException:
        pass
    
    exec(action, globals(), locals())

def OnParserSkip( string, location, tokens ):
    """
    Skips the matching line
    """
    pass

def OnParserEmit( string, location, tokens ):
    """
    Passes the given string
    """
    line = tokens.get('tag')
    print line
    
    

def OnParserInline( string, location, tokens ):
    """
    Handles parser Keep directive
    """
    pass

def OnCppMacro( string, location, tokens ):
    """
    Passes CPP macros straight into the output file
    """
    syntax.characters(string)


# A stack to keep track of the context of which type of method
# we are in.  Here, a method is a Module, , Subroutine or
# Function... i.e. anything which is terminated with a simple
# END statement.
_method_stack = []

def OnModule( tokens ):                                                  # Open tag
    """
    Interface to the XML Module tag handler
    """
    syntax.startElement( 'Module',  tokens.asDict() )
    __in_module__ = True
    _method_stack.append( 'Module' )

def OnModuleEnd( tokens ):
    """
    Interface to the XML End Module tag handler.  Here,
    we will actually manage closing of any Module, Function
    or Subroutine.
    """
    syntax.endElement( _method_stack.pop() )
    __in_module__ = False
    __finished__  = True

def OnBlock( tokens ):
    """
    Interface to the XML Block handler
    """
    name = tokens.get('name')
    comm = tokens.get('comment',None)
    comment = ' '#.join(comm)
    if comm:        comment = ' '.join(comm)
    else: comment = 'is a block that an author failed to adequately document'
    syntax.startElement('Block', {'name':name, 'comment':comment})

def OnBlockEnd( tokens ):
    syntax.endElement('Block')
    
def OnSubroutine( tokens ):
    """
    OnSubroutine expects a 'SUBROUTINE' keyword, followed by the name
    of the subroutine and any arguements to the subroutine.
    """
    call = tokens[0] # Must always be SUBROUTINE
    name = tokens[1]
    args = tokens[2:]
    syntax.startElement( 'Subroutine', {'name':name, 'args':args} )
    _method_stack.append('Subroutine')

def OnFunction( tokens ):
    pass    
       
def OnCreated( tokens ):                                                 # One line
    """
    Interface to he XML Created tag handler
    """
    syntax.startElement( 'Created', tokens.asDict() )
    syntax.endElement  ( 'Created' )

def OnAuthor( tokens ):                                                  # One line
    """
    Interface to the XML Author tag handler
    """
    attr = { 'name' : ' '.join( tokens.asDict()['name'] ) }
    syntax.startElement( 'Author', attr )
    syntax.endElement  ( 'Author' )

def OnCmzList( tokens ):
    """
    Interface to the XML CDE tag handler
    """
    syntax.startElement('CDE',{})                   # open
    for include in tokens.asDict()['list']:         # content
        syntax.characters(include)   
    syntax.endElement('CDE')                        # close

def OnContent( tokens ):
    """
    Interface to the content delcaration
    """
    syntax.startElement('Content',{})
    syntax.characters(','.join(tokens.get('list')))
    syntax.endElement('Content')

def OnStructure( tokens ):
    """
    Interface to the XML Structure tag handler
    """
    
    syntax.startElement( 'Structure', {'name' : tokens.asDict()['name']} )
    vars = tokens.get('variables')

    # Merge toghether 2D arrays
    myvars = ""
    open = False

    # Loop over each variable
    for var in vars:

        # Loop over all characters and append to myvars
        for c in var:
            myvars += c
            # If we encounter an ( we open paren
            if c == "(": open=True
            if c == ")": open=False

        # Separate complete variables with a ; 
        if open==False:            myvars += ';'
        # Comma if we are w/in a paren
        else:                      myvars += ','

    vars = myvars.split(';')
    
    i=0
    while (i<len(vars)):

        if len(vars[i])==0:
            i+=1
            continue

        attr = { 'type':'float',
                 'name':vars[i] }

        if ( vars[i].lower()=='int' or
             vars[i].lower()=='char' ):

            mytype=vars[i].lower()
            i+=1
            myvari=vars[i].lower()

            if len(myvari.strip())>0:
                attr['type']=mytype
                attr['name']=myvari

        # Filter out null characters
        if len(var.strip())>0:
            syntax.startElement( 'var', attr )
            syntax.endElement('var')
            
        i+=1
        
    syntax.endElement('Structure')

def OnVarlist(tokens):
    """
    OnVarList is executed when a variable-list is matched
    """
    global _method_stack

    type = tokens.get('type').lower()
    list = tokens.get('list')

    dim  = tokens.get('dim',None)
    len  = tokens.get('len',None)

    bytes = tokens.get('bytes')

    if ( bytes != None ):
        if type == 'character': type += bytes

    
    attr = { 'type': type.upper() }
    
    if ( dim != None ): attr['dim']=dim
    if ( len != None ): attr['len']=len
    
    #
    # Examine the method stack.  Within a Module, we will route the
    # input through the varlist exporter.  Otherwise, we will create
    # a new Arguement 
    # 
    _method_stack.reverse()
    
    #
    # Obtain the context in which the variable list is declared.  This
    # should be either module, subroutine or function
    #
    context = _method_stack[0]
    _method_stack.reverse()
    
    #
    # We can have both arguement-variables and standard variables interleved
    # with each other when parsing fortran.  So we sort those out at this
    # point.  This is done by examining the list of arguements in the current
    # parent
    #
    args   = []
    myargs = []
    myvars = []
    if context == 'Subroutine' or context == 'Function':
        args = syntax.top().args
        
    for a in list:
        if a in args: myargs.append(a)
        else:         myvars.append(a)
        
    # Route arguements through the <Arguement ... /> handler
    for a in myargs:
        syntax.startElement ( 'Arguement',{'name':a, 'type':type, 'intent':'inout'} )
        syntax.endElement   ( 'Arguement' )
 
    if myvars:       
        syntax.startElement ( 'varlist',attr )
        syntax.characters   ( ','.join(myvars) )
        syntax.endElement   ( 'varlist' )

        
    

def OnParameter( tokens ):
    """
    Interface to the XML Parameter tag handler
    """
    list = tokens.get('list')
    for par in list:
        (name,value)=par.split('=')
        name=name.strip()   # get rid of the whitespace
        value=value.strip() # ditto
        syntax.startElement('Parameter', {'name': name, 'type': None, 'value' : value })
        syntax.endElement('Parameter')

def OnInline( tokens ):
    """
    Interface to the XML Inline function tag handler
    """
    #out( tokens.asDict() )
    name    = tokens.get('name')
    arglist = tokens.get('arglist')
    expr    = tokens.get('expr')
    attr    = {'name':name,
               'arglist':','.join(arglist),
               'expr':''.join(expr) }
    syntax.startElement('Inline',attr)
    syntax.endElement('Inline')
    
def OnFill( tokens ):
    global __in_fill__
    if ( __in_fill__ ):        OnFillEnd( tokens ) # end previous fill if we are in one
    
    name    = tokens.get('name')
    comment = ' '.join( tokens.get('comment') )    
    syntax.startElement( 'Fill', { 'name':name, 'comment':comment } )
    __in_fill__ = True # set the in fill flag = true
    # Replace the assignment rule
    RULES['Assignment']=AgFillAssignment

def OnFillEnd( tokens ):
    global __in_fill__
    __in_fill__ = False
    syntax.endElement('Fill')
    # Restore the assignment rule    
    RULES['Assignment']=AgAssignment

def OnWhite( s, loc, tokens ):
    """
    OnWhite is the action taken when whitespace is encountered in the
    AgSTAR file.  Probably should just pass it along...
    """
    #print s # Print the original string
    pass


def OnFillAssignment( tokens ):
    """
    Handles parsing of assignments in fills.  Fill assignments match variable = restOfLine.
    The rest of the line will be parsed in the OnFillAssignment routine.  This parsing will
    attempt to match first an expression, then a character assignment, and finally an array
    or matrix (2d array) assignment.
    """
    name = tokens.get('name')
    line = tokens.get('line')

    rule_expr  = OneOrMore( Word(alphanums+'.+-/*()_ ') )
    rule_char  = Literal("'") + Word(alphanums) + Literal("'")
    #rule_array = lbrack + delimitedList( Word(alphanums+'.+-/*()_ ' ), combine=True ) + rbrack
    #rule_matrx = lbrack + Combine( OneOrMore( delimitedList( Word(alphanums+'.+-/*()_ ' ),combine=True ) + semi ) ) + rbrack

    rules = [ rule_expr,
              rule_char ]

    # First try matching expression and character assignments
    for r in rules:
        r = r ('value') + Literal('!').suppress() + restOfLine('comment')
        try:
            result=r.parseString(line)
            values = result['value']
            for i,v in enumerate(values):
                values[i] = v.strip()       # strip whitespace            
            value   = ''.join( values )
            value   = value.replace('; ',';\n')
            
            value   = value.strip()
            comment = result['comment']
            attr = {'name'    : name,
                    'value'   : value,
                    'comment' : comment }
            syntax.startElement('var',attr)
            syntax.endElement('var')
            return
        
        except ParseException:
            pass

    # Next we might be dealing with 1- or 2-dim arrays
    rule_array = (
        lbrack.suppress() +
        delimitedList( Word(alphanums+'.+-/*()_ '), delim=(comma) )  ('list') +
        rbrack.suppress() + '!' + restOfLine('comment')
        )

    try:        
        result = rule_array.parseString(line)
        value  = '{' + ','.join( result.get('list') ) + '}'
        comment = result.get('comment')
        attr = {'name':  name,
                'value': value,
                'comment' : comment }

        syntax.startElement('var',attr)
        syntax.endElement('var')
        return
    except ParseException:
        pass

    # Finally we try the multi-dimensional form
    rule_array = (
        lbrack.suppress() +
        delimitedList( Word(alphanums+'.+-/*()_, '), delim=(semi) ) ('list') +
        rbrack.suppress() + '!' + restOfLine('comment')
        )

    try:        
        result = rule_array.parseString(line)
        value  = '{' + ';'.join( result.get('list') ) + '}'
        comment = result.get('comment')
        attr = {'name':  name,
                'value': value,
                'comment' : comment }

        syntax.startElement('var',attr)
        syntax.endElement('var')
    except ParseException:
        pass        
    

def OnUse( tokens ):
    """
    Interface ot the XML Use tag handler
    """
    attr = {}
    attr['struct']=tokens.get('struct')
    selector = tokens.get('select')
    if ( selector != None ):
        (select,value)=selector.split('=')
        attr['select']=select
        attr['value']=value
    syntax.startElement('Use', attr )
    syntax.endElement('Use')

def OnAssignment( s, loc, tokens ):
    # Sends output through the top syntax handler on the stack
    syntax.characters( s )


def OnPrint( tokens ):
    """
    Interface to the XML Print tag handler.
    """
    level   = tokens.get('level')
    content = ''
    if ( tokens.get('list') != None ):
        content = ','.join( tokens.get('list')   )
    format  = ' '.join( tokens.get('format') )
    syntax.startElement('Print', {'level':level, 'fmt': format } )
    syntax.characters(content)
    syntax.endElement('Print')

def OnComment( tokens ):
    """
    Interface to the XML Comment handler
    """
    syntax.startElement('Comment', {'type': tokens.get('type')})
    comment = tokens.get('comment')
    if ( comment != None ):
        syntax.characters(' '.join(comment))
    syntax.endElement('Comment')

def buildAttr( tokens ):
    """
    helper function to build an attribute list out of
    AgATTR's stored in the 'list' element of tokens.
    """
    list=tokens.get('list')
    attr = {}
    if ( list != None ):
        for a in list:
            (key,value)=a.split('=')
            key=key.strip()             # no whitespace in keys
            attr[key.lower()]=value
    return attr
    
def OnMaterial( tokens ):
    name = tokens.get('name')
    list = tokens.get('attributes')
    attr,mykeys,myvals = parseAttributeList( list )
    attr['name'] = name
    syntax.startElement('Material',attr)
    syntax.endElement('Material')
   

            
    #attr = buildAttr(tokens)
    #attr['name']=name
    #syntax.startElement('Material',attr)
    #syntax.endElement('Material')

def OnMedium( tokens ):
    name = tokens.get('name')
    attr = buildAttr(tokens)
    attr['name']=name
    syntax.startElement('Medium',attr)
    syntax.endElement('Medium')


def findVarStart( mychars, ieq ):
    ivar = 0 # falls off front of string

    toggled  = False   # wait until first non whitespace character
    i=ieq-1            # start one to the left of the equals sign
    while ( i >= 0 ):
        c = mychars[i]
#        print "%04i | %s"%( i, c )
        
        if ( c != ' ' ):
            toggled=True

        if ( toggled ): # We are in a variable searching for its start
#            print 'toggled'
            if ( c == ' ' ): # this will be the leading whitespace
#                print 'break'
                ivar = i+1
                break        
        i -= 1

    return ivar

def preStripLine( line, LHS=True, RHS=False ):

    line=', '.join( line.split(',') ) # needed to separate key=value pairs from different lines

    mychars    = []
    equals     = []
    variables  = []
    i = 0
    for c in line:
        mychars.append( c )    # add te character to the mychars array
        if ( c == '=' ):
            equals.append( i ) # Record the position of every equals sign
        i+=1
    mychars.append(' ')

    for ieq in equals:
        ivar = findVarStart( mychars, ieq )
        variables.append(ivar)

    myline=""
    for i,c in enumerate(mychars):

        for jvar in variables:          # append a space at the start of 
            if ( i==jvar ): myline += ' '  # every variable

        if ( c != ' ' ):                # but otherwise we are stripping
            myline+=c                   # whitespace from the line

    return myline    

def parseAttributeList( line ):
    """
    Parses the attribute list of geant operators to find key=value pairs.
    Code exhibits odd behavior in that konly='MANY' or 'ONLY' will terminate
    the parsing, so these flags need to appear at the end.
    """

    line = preStripLine(line)               # strips whitespace from the line, leaving whitespace between
                                            # key=value pairs
    MANY=Combine( squote+CaselessKeyword('MANY')+squote )
    ONLY=Combine( squote+CaselessKeyword('ONLY')+squote )
    EXPR=Word( alphanums+'_.+-*/{()},' )

    rule = ( Word(alphanums+'_')             ('LHS')  +
             equal                                    +
             (MANY|ONLY|EXPR)                ('RHS')  +
             restOfLine                      ('REST')  
             ) 

    tokens = {}
    keys   = []
    values = []

    try: 
        result = rule.parseString(line)

        key = result['LHS'].lower()   
        val = result['RHS'].strip(',')

        tokens[key] = val   # add key,value pair to attribute dictionary
        keys.append(key)    # add key to list of keys
        values.append(val)  # add value to list of values

        mytokens,mykeys,myvals = parseAttributeList( result.get('REST') )

        for i,key in enumerate(mykeys):

            val = myvals[i].strip(',')
            key = key.lower()
            
            tokens[key] = val
            keys   . append(key)
            values . append(val)
        
    except ParseException:
        return {},[],[] # Return a null list

    return tokens,keys,values
    

           
def OnShape( tokens ):
    type = tokens.get('type')
    line = tokens.get('line')

    attr,mykeys,myvals = parseAttributeList(line)

    try:                       # shapes.py expects npdiv not npdiv
        npdiv=attr['npdv']
        attr['npdiv']=npdiv
    except KeyError:
        pass
    
    attr['type']=type
    
    syntax.startElement('Shape',attr)
    syntax.endElement('Shape')

def OnInlinedComment( tokens ):
    pre = tokens.get('pre','')
    post = tokens.get('post','')
    #comment = '! '+tokens.get('comment')
    #line1=comment
    line2=pre+post
    #tryRules( line1.rstrip(';') )
    tryRules( line2.rstrip(';') )
    

def OnAttribute( tokens ):
    block = tokens.get('for')
    line = tokens.get('line')
    attr,mykeys,myvalues=parseAttributeList(line)
    attr['for']=block
    syntax.startElement('Attribute',attr)
    syntax.endElement('Attribute')

#    attr = buildAttr(tokens)
#    attr['for']=block
#    syntax.startElement('Attribute',attr)
#    syntax.endElement('Attribute')

components = []
def OnMixture( tokens ):
    global components
    name = tokens.get('name')
    list = tokens.get('list')
    attr,mykeys,myvals = parseAttributeList(list)
    attr['name']=name
    syntax.startElement('Mixture',attr)    
    for attr in components:
        syntax.startElement('Component',attr)
        syntax.endElement('Component')
    syntax.endElement('Mixture')
    components = []

def OnComponent(tokens):
    name = tokens.get('name')
    list = tokens.get('attributes')
    attr,mykeys,myvals = parseAttributeList(list)
    attr['name']=name
    components.append(attr)
    
_loop_stack = []

def OnLoop( tokens ):
    """
    Interface to the XML do (aka for) loop construct
    """
    var = tokens.get('variable')
    low = ' '.join(tokens.get('lower'))
    up  = ' '.join(tokens.get('upper'))
    stp = tokens.get('stride')
    attr = {'var':var,
            'from':low,
            'to':up }
    if ( stp != None ):
        if ( stp != '1' ):
            attr['step']=' '.join(stp)
    syntax.startElement( 'Do', attr )

    _loop_stack.append('Do')

def OnWhile( tokens ):
    """
    """
    attr = {'expr' : tokens.get('expr') }
    syntax.startElement('While', attr )

    # add 'While' to the loop stack
    _loop_stack.append('While')

def OnLoopEnd(tokens):
    syntax.endElement( _loop_stack.pop() )

def OnReplaceMatch( s, loc, tokens ):
    #print "! ==================================================================== OnReplaceMatch == "
    #print tokens
    #form( _current_line ) # ILLEGAL!!!! NEEDS TO GO THROUGH SYNTAXHANDLER!!!
    #print s
    #syntax.Language.form( s )
    #print s.lstrip()
    if ( s.lstrip() != '\n' ):
        syntax.characters( s.lstrip() )

def OnReplace(s, loc, tokens):
    global _current_line

    # First handle the required output tag
    match = ' '.join(tokens.get('match'))
    attr  = {'match':match}
    syntax.startElement('Replace',attr)
    wit = ' '.join(tokens.get('with')).split(';')
    for w in wit:
        if ( len(w.strip())>0 ):
            syntax.characters(w+';')
    syntax.endElement('Replace')

    # Next define the rule needed to match
    # the target macro and the action to
    # take
    RULE = makeRuleFromReplace( match )
    addRule( match, RULE, OnReplaceMatch )



def OnCheck(tokens):
    expr = tokens.get('expr').strip(';')
    syntax.startElement('Check',{'expr':expr})
    syntax.endElement('Check')
    

def OnCreate(tokens):
    block = tokens.get('block')
    line  = tokens.get('line')
    attr,mykeys,myvals = parseAttributeList( line )
    attr['block']=block
    syntax.startElement('Create',attr)
    syntax.endElement('Create')



def OnPosition(tokens):

    block  = tokens.get('block')
    mother = tokens.get('mother')
    line   = tokens.get('line')

    myattr,mykeys,myvals = parseAttributeList( line )

    # Form attributes for placement directive
    place_attr = { 'block' : block,
                   'in'    : mother }

    #for key in ['x','y','z','konly','ncopy']:
    for key in mykeys:

        # Skip rotation and orientation keys
        if ( key in ['thetax','thetay','thetaz','phix','phiy','phiz','alphax','alphay','alphaz','ort'] ): continue
                
        val = myattr.pop(key,None)
        if ( val != None ):
            place_attr[ key ] = val

    syntax.startElement('Placement',place_attr)

    # Loop over keys in the order which they appeared

    mykeys.append('')
    myvals.append('')
    
    ikey=0
    reference = []
    
    while ( ikey<len(mykeys) ):

        key = mykeys[ikey]
        val = myvals[ikey]

        # Collect up the arguements which specify a change of axis
        axes = {}
        while ( key in ['thetax','thetay','thetaz','phix','phiy','phiz'] ):
            axes[ key ] = val
            ikey +=1
            key = mykeys[ikey]
            val = myvals[ikey]

        # Handle definition of the reference frame
        if axes != {}:
            syntax.startElement('Rotation',axes)
            syntax.endElement('Rotation')
            axes = {}

        # Handle rotations around one or more axes
        if ( key in ['alphax','alphay','alphaz','ort'] ):
            syntax.startElement('Rotation',{key:val})
            syntax.endElement('Rotation')

        
        ikey+=1
        
            
        
    syntax.endElement('Placement')

def OnCreateAndPosition(tokens):

    block  = tokens.get('block')
    line   = tokens.get('line')
    mother = tokens.get('mother')

    myattr,mykeys,myvals = parseAttributeList( line )
    
##     position_flags = ['x','y','z','alphax','alphay','alphaz','thetax','thetay','thetaz',
##                       'phix','phiy','phiz','ort','konly','ncopy']

    create_attr   = {'block':block,'mother':mother}
    position_attr = {'block':block,'mother':mother}

    create_line = ""
    position_line = ""

##     for key in mykeys:
##         val = myattr[key]
##         if key.lower() in position_flags:
##             position_line += "%s=%s "%( key, val )
##         else:
##             create_line += "%s=%s "%( key, val )

    # Associate ALL arguements with position
    for key in mykeys:
        val = myattr[key]
        position_line += "%s=%s "%(key, val)
    

    create_attr['line']=create_line
    position_attr['line']=position_line

    OnCreate( create_attr )
    OnPosition( position_attr )

    #out( position_attr )
    #out( create_attr )

    
            



def feather( nest, opener='(', closer=')', strip=False ):
    """
    Input is a nested expression where every list
    should be wrapped in ( ) and concatenated
    toghether.  A list w/in the nest should be
    recurssed to produce ( .. ( .. ( ..) ) )
    type structure.
    """

    mystr=''
    if not strip:    mystr+=opener

    for element in nest:

        if ( type(element)==type('') ):   # if this is a string
            mystr+=element
        elif ( type(element)==type([]) ): # if this is a list
            mystr+=feather( element )

    if not strip:    mystr+=closer

    return mystr


# if/then/else stack to keep track of the nesting of if blocks
ite_stack = []

def HandleMortranExpression( expr ):
    #print 'ageParser| %s'%expr
    return expr


def OnIfBegin(tokens):
    expr = tokens['expr']
    myexpr = feather(expr.asList(),strip=True)
    myexpr = myexpr[ 1:len(myexpr)-1 ]
    myexpr=HandleMortranExpression( myexpr )
    syntax.startElement( 'If', {'expr': myexpr} )
    
    ite_stack.append('IF') # Add IF to the top of the stack



    
def OnIfEnd(tokens):
    current = ite_stack[ len(ite_stack)-1 ]
    if current == 'ELIF':  # Close the preceding ELIF block and then the IF block
        OnElseIfEnd(tokens)
        OnIfEnd(tokens)
    elif current == 'ELSE':  # Close the preceding ELSE block
        OnElseEnd(tokens)
        OnIfEnd(tokens)
    elif current == 'IF':
        syntax.endElement( 'If' )
        ite_stack.pop() # pop the stack
    else:
        print "!========================================================================== UNKNOWN =="

def OnElseIfBegin(tokens):
    expr = tokens['expr']
    myexpr = feather(expr.asList(),strip=True)
    myexpr = myexpr[ 1:len(myexpr)-1 ]    

    myexpr=HandleMortranExpression( myexpr )    
    syntax.startElement( 'Elif', {'expr': myexpr } )
    top = ite_stack[len(ite_stack)-1]
    if ( top != 'IF' and top != 'ELIF' ):
        print "! ============================================================== ELSEIF/IF mismatch =="
    ite_stack.append('ELIF')
    
def OnElseIfEnd(tokens):
    syntax.endElement('Elif')
    if ite_stack.pop() != 'ELIF': # Pop the stack and warn if it's not an ELIF
        print "! =========================================================== ELSEIF/close mismatch =="

def OnElseBegin(tokens):
    if ite_stack[len(ite_stack)-1] == 'ELIF':        OnElseIfEnd(tokens)
    elif ite_stack[len(ite_stack)-1]=='IF':          pass
    else:
        print "!========================================================================== UNKNOWN =="
    syntax.startElement('Else', {} )        
    ite_stack.append('ELSE') # Add the ELSE to the top of the stack

def OnElseEnd(tokens):
    syntax.endElement('Else')
    if ite_stack.pop() != 'ELSE': # Pop the stack and warn if it's not an ELSE
        print "! =========================================================== ELSEIF/close mismatch =="        

def OnIfAssign(tokens):
    expr = feather( tokens['expr'].asList(), strip=True )
    assign = tokens.get('assign')
    #print expr
    #print assign
    syntax.startElement('If', {'expr':expr})
    #syntax.characters(' '.join(assign))
    syntax.characters(assign)
    syntax.endElement('If')
    

def OnReturn(tokens):
    syntax.startElement('Return',{})
    syntax.endElement('Return')

def OnGSTPar(tokens):

    name = tokens.get('name')
    valu = tokens.get('value')
    
    syntax.startElement('Par',{'name':  name,
                               'value': valu } )
    syntax.endElement('Par')

def OnCall(tokens):
    routine = tokens.get('routine')
    expr   = tokens.get('expr')
    myexpr = feather(expr.asList(),strip=True).lstrip('(').rstrip(')')

    syntax.startElement('Call', { 'routine' : routine,
                                  'expr'    : myexpr })
    syntax.endElement('Call')

def OnIncrement(tokens):
    var = tokens.get('name')
    op  = tokens.get('operator')
    ex  = tokens.get('expr')
    syntax.characters('%s %s %s'%(var, op, ex))

def OnHits(tokens):
    """
The complete format of HITS(DIGI) operator is

           HITS name  measurement[=value]:bin or bit:OPTION(min,max) ...
    """
    MEAS = ( Word(alphanums+'_=')                                             ) ('meas')
    BITS = ( Word(nums)                                                       ) ('nbits')
    BINS = ( Combine(Optional(Word(nums)) + Literal('.') + Optional(Word(nums)) )       ) ('bins')
    OPTS = ( CaselessLiteral('R')|CaselessLiteral('C')|CaselessLiteral('S')   ) ('opts')
    MNMX = ( Literal('(') + Word(alphanums+'_')('min') + Literal(',') + Word(alphanums+'_')('max') + Literal(')'))

    RULE = MEAS + Literal(':') + (BITS|BINS) + Literal(':') + Optional(OPTS) + Optional(MNMX)
             

    
    name=tokens.get('name')
    attr = {}
    attr['for']=name
    hits=tokens.get('definition')

    type = tokens.get('type',None)
    if ( type != None ):
        list=type.split('=')
        attr['type'] = list[1]

    list=hits.split(' ')
    if type != None:
        list.append( type )
    

    syntax.startElement('Instrument', {'block' : name } )
    
    for i,hit in enumerate(list):

        hit=hit.lower().strip()        
        if ( len(hit)>0 ):

            result = RULE.parseString(hit)

##             print result.get('meas',None)
##             print result.get('bins',None)
##             print result.get('nbits',None)
##             print result.get('opts',None)
##             print result.get('min',None)
##             print result.get('max',None)
                        
##             ##key,value=hit.split(':',1)                        
##             ##key=key.lower()
##             ##value=value.upper()
##             ##value=value.strip(',')            
##             ##attr[key]=value

##             mylist = hit.split(':') 
##             mylist.append(None)

##             args = {}
            
##             args['meas'] = mylist[0]
##             borb = 'nbits'
##             args[ borb ] = mylist[1]

##             try:
##                 if mylist[2]:
##                     mymin,mymax = mylist[2].strip('(').strip(')').split(',')
##                     args['min']=mymin
##                     args['max']=mymax
##             except:
##                 print mylist
##                 assert(0)

            syntax.startElement('Hit', result )
            syntax.endElement('Hit')
            
            

            

    syntax.endElement('Instrument')
            
            
##    syntax.startElement('Hits',attr)
##    syntax.endElement('Hits')

def OnExternal( tokens ):
    list = tokens.get('list')
    for ex in list:
        syntax.startElement('External',{'routine':ex})
        syntax.endElement('External')

def OnData( tokens ):

    global syntax_type
    variables = tokens.get('variables')
    values    = tokens.get('values')

    variables = ','.join(variables)
    values    = ','.join(values)
    
    syntax.startElement('Data', { 'variables' : variables,
                                  'values'    : values    } )
    syntax.endElement('Data')
    
def setActions():

    ParserAction . setParseAction ( OnAction  )
    ParserSkip   . setParseAction ( OnParserSkip )
    ParserInline . setParseAction ( OnParserInline )
    ParserEmit   . setParseAction ( OnParserEmit )

    CppMacro     . setParseAction ( OnCppMacro )

    AgModule     . setParseAction ( OnModule  )
    AgCreated    . setParseAction ( OnCreated )
    AgAuthor     . setParseAction ( OnAuthor  )
    AgInlinedComment.setParseAction( OnInlinedComment )
    AgComment    . setParseAction ( OnComment )
    AgCmzList    . setParseAction ( OnCmzList )
    AgContent    . setParseAction ( OnContent )
    AgModuleEnd  . setParseAction ( OnModuleEnd )    
    AgStructure  . setParseAction ( OnStructure )
    AgVarlist    . setParseAction ( OnVarlist   )
    AgParameter  . setParseAction ( OnParameter )
#   AgInline     . setParseAction ( OnInline    )
    AgFill       . setParseAction ( OnFill      )
    AgWhiteSpace . setParseAction ( OnWhite     )
    AgFillEnd    . setParseAction ( OnFillEnd   )
    AgFillAssignment . setParseAction( OnFillAssignment )
    AgAssignment     . setParseAction( OnAssignment )
    AgCharAssignment . setParseAction( OnAssignment )
    AgArrayAssignment    . setParseAction( OnAssignment )
    AgArrayElementAssignment . setParseAction( OnAssignment )
    AgUse            . setParseAction( OnUse )
    RULES['Print'] = AgPrintFmt     # Change print matching rule
    AgPrintFmt    . setParseAction( OnPrint )
    AgComment     . setParseAction( OnComment )
    AgBlock       . setParseAction( OnBlock )
    AgBlockEnd    . setParseAction( OnBlockEnd )
    AgMaterial    . setParseAction( OnMaterial )
    AgMedium      . setParseAction( OnMedium   )
    AgShape       . setParseAction( OnShape    )
    AgComponent   . setParseAction( OnComponent )
    AgMixture     . setParseAction( OnMixture )
    AgAttribute   . setParseAction( OnAttribute )
    AgLoopBegin   . setParseAction( OnLoop )
    AgWhileBegin  . setParseAction( OnWhile )
    AgLoopEnd     . setParseAction( OnLoopEnd )
    AgReplaceFull . setParseAction( OnReplace )
    AgCreate      . setParseAction( OnCreate )
    AgPosition    . setParseAction( OnPosition )
    AgCreatePos   . setParseAction( OnCreateAndPosition )
    AgIfBegin     . setParseAction( OnIfBegin )
    AgIfEnd       . setParseAction( OnIfEnd )
    AgIfAssign    . setParseAction( OnIfAssign )
    AgElseIf      . setParseAction( OnElseIfBegin )
    AgElse        . setParseAction( OnElseBegin )
    AgReturn      . setParseAction( OnReturn )
    AgGSTPar      . setParseAction( OnGSTPar )
    AgCall        . setParseAction( OnCall   )
    AgIncrement   . setParseAction( OnIncrement )
    AgHits        . setParseAction( OnHits )
    AgCheck       . setParseAction( OnCheck )
    AgExternal    . setParseAction( OnExternal )
    AgData        . setParseAction( OnData )
    AgSubroutine  . setParseAction( OnSubroutine )
    AgFunction    . setParseAction( OnFunction )

def main( opts, args ):

    global _current_line, print_line, syntax, syntax_type

    # Create the syntax handler which is responsible for output
    syntax = SyntaxHandler( opts )
    syntax_type = opts.language

    # Open the geometry file and read it into the working buffers
    readFile( opts.filename )

    # Chop the buffer into lines
    geomLines = chopLines(buffer1)

    # Group the lines into statements
    geomState = groupStats( geomLines )

    # Group replace [..] with [..] blocks into single lines
    geomState = groupReplace( geomState )

    # Group statements containing matrix definitions
    geomState = groupMatrix( geomState )

    # Associate actions with the rules defined in Rules.py
    setActions()

    # Prepend document tag
    syntax.startElement('Document',{'file': opts.filename})

    # Loop over each line trying each rule.  The actions defined
    # above will interface the first successful match with the
    # syntax handler.
    iline=0
    for iline,line in enumerate(geomState):        

        if ( __finished__ ): break # may be raised by OnModule

        _current_line = line

        if ( print_line ): print line
        
        tryRules( line.strip(';') )
        if ( line == '' ): print


    msg=[]
    
    msg.append( "!$ =======================================================================================" )
    msg.append( "!$ ageParser finished" )
    msg.append( "!$ number of warnings: %i"%numberOfWarnings() )
    if ( len(syntax.stack) > 2 ):
        msg.append( "!$" )
        msg.append( "!$ WARNING: syntax handler has objects left on its stack.  This" )
        msg.append( "!$          may indicate that code blocks are not properly nested" )
        msg.append( "!$          and/or closed.  Expect compilation problems." )
        msg.append( "!$" )
        msg.append( "!$          If the output is terminated by ENDFILL instead of END," )
        msg.append( "!$          the easy solution is to add ENDFILL to the end of each" )
        msg.append( "!$          FILL block in the input file." )
        msg.append( "!$" )
    
    # And end the document tag
    syntax.endElement('Document')

    for s in syntax.stack:
        msg.append( "!$ " + str( s ) )

    for m in msg:
        AgComment.parseString(m)




        
#    if ( iline<len(geomState) ):
#        print "!$ WARNING: early termination of parser with additional statements"
#        print "!$          left to be processed.  Statements are:"
#        while iline < len(geomState):
#            print "!$ "+geomState[i]
#    print ""

        

if __name__ == '__main__':

    # Parse command line to obtain the filename of the
    # geometry being translated
    optparser = OptionParser()
    
    optparser.add_option("--file",   help="Selects the geometry file to be parsed.",        dest="filename", default="NONE")
    optparser.add_option("--export", help="Selects the export language (Mortran,AgML,...)", dest="language",default="AgML")
    optparser.add_option("--module", help="Defines the name of the module for export",      dest="module_name", default=None)
    optparser.add_option("--path",   help="Defines the path to export the files",           dest="path_name",   default='./')
        
    (opts, args) = optparser.parse_args()

    main( opts, args )
        


    


