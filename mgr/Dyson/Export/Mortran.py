from Handler import Handler

import Dyson.Utils.Shapes
from   Dyson.Utils.Shapes import shape_params

import os

export_comments = True

_agstar_attribute_list = ['seen','colo','ltyp','serial','fill','lsty','lwid'];

_structures = {}

_struct_sep = '_'
_agml_sep = ':'
_soft_limit = 80 # applies a soft limit on the line length
_depth = 0
_prepend = ''

_symbol_table = {}

_isExporting = True

from pyparsing import *

# locator object which implements getLineNumber() and possibly other methods
# to be set by the syntax handler when the language is defined
locator = None

# ====================================================================================================
class NotYet( Exception ):
    def __init__(self):
        pass
    def __str__(self):
        return repr(self)+": Not yet implemented"

# ====================================================================================================
class SimplePrint:
    def __init__(self):
        pass
    def __call__(self,line,indent=True):
        print "    "+line

class NoPrint:
    def __init__(self):
        pass
##    def __call__(self,line,indent=True):
    def __call__(self,line,**kw):
        pass
    ##print "!!// "+line    

# ====================================================================================================
type_map = {
    'int'     : 'integer',
    'Int_t'   : 'integer',
    'float'   : 'real',
    'Float_t' : 'real'
    }



class PrettyPrint:

    """class Formatter
    
    Provides pretty (and uniform) printing of Mortran source code.  We will impose some
    (harsh) rules on the format of the output files.  Sorry but they will become quite
    unreadable if people have complete freedom to format code the way they want.  The
    XML parser will insert blank lines seemingly at random, indentation levels will not
    be consistent between the AgML and moretran source, etc...  So we need to take steps
    to format nicely

    """

    margin_l = 0    # Mortran is semi-fixed format.... left margin is gone, right remains
    margin_r = 72
    
    def __init__(self):
        self.level = 2
        self.indent = "   "

    # ================================================
    #
    # Function which does the printing
    def __call__(self, line, indent=True, cchar=',', lstrip=True, debug=False, breakers=' ,', limit=_soft_limit ):

        self.limit = limit

        if ( debug ):
            print 'Pretty Print called with: '
            print line
            print ''
        

        if ( indent and lstrip ):
            line = line.lstrip()    # remove whitespace on left
        #self.sepline(line)         # print separators

        if ( indent ):
            self.decrease(line)         # decrease indentation level
                
        myline = ""                     # indent to proper level
        i=0
        if ( indent ):
            while (i<self.level):
                myline+=self.indent
                i+=1

        if ( debug ):
            print '!' + myline + '^'

        # Perform replacement of structure tags
        for search,replace in _structures.iteritems():            
            line = line.replace( search.lower(),
                                 replace.upper() )

        if '#include' in line:
            self.cprint (       line,cchar,debug,breakers)  # without indentation
        else:        
            self.cprint (myline+line,cchar,debug,breakers)  # print the line
        
        #self.spcline(line)          # add space behind line
        if ( indent ):
            self.increase(line)

        #self.update(line)
    #
    # ================================================        
    def spcline(self,line):
        spacers = ['MODULE','BLOCK','SUBROUTINE','END','SHAPE','}!','MIXTURE','use ']
        for spc in spacers:
            try:
                index = line.index(spc)
                print ""
            except ValueError:
                pass
                   
                   
    def sepline(self,line):
        """
        prints a separation line on certain constructs
        """
        seps = ['MODULE ','SUBROUTINE ','BLOCK ','FUNCTION']
        for s in seps:
            try:
                index = line.index(s)
                print "c --------------------------------------------------------------------------------"
            except ValueError:
                pass

    def cprint(self,line,cchar,debug,        breakers = ' ,' ):
        """
        conditional print.  line is not printed if empty.
        cchar is(are) the continuation character(s) appended at the
        end of continuation lines
        """

        if ( debug ): print 'cprint got '+ line
        
        # Require non null characters
        if ( not len(line.strip()) ): return

        outline = ''
        
        atlimit = False
        incomm  = False
        
        for i,char in enumerate(line.rstrip()):

            # Check for comments
            if ( char == '!' ): incomm = True

            if ( i==0 and char == '*' ): incomm = True

            # When we hit a soft limit, raise the
            # soft limit flag (but only if we are
            # not w/in a comment line
            if ( i and not i % self.limit ):
                atlimit = not incomm

            # If we are at a soft limit, we will replace
            # the next ' ' with a '\n' + level+1 indents
            if ( atlimit ):

                if ( debug ): print '! at limit is true'

                for b in breakers:
                    if ( char == b ):
                        atlimit = False
                        char = cchar + '\n' + _prepend
                        j=0
                        while ( j<self.level+2 ):
                            char += self.indent
                            j+=1

            outline += char

        # And finally output the line we have built up
        print _prepend + outline
        

    def increase(self,line):
        """
        increases or decreases the indentation level when certain key
        words are detected on the line
        """
        incs = ['MODULE','SUBROUTINE','DO','THEN','ELSE','BLOCK','FILL']
        for inc in incs:
            try:
                index = line.index(inc)
                #$$self.level += 1
            except ValueError: # substring not found
                pass

    def decrease(self,line):
        """
        increases or decreases the indentation level when certain key
        words are detected on the line
        """
        decs = ['END','ENDFILL','ENDBLOCK!','ENDDO']        
        for dec in decs:
            try:
                index = line.index(dec)
                #$$self.level -= 1
            except ValueError: # substring not found
                pass             

    def expand(self,line):
        """
        Inserts spaces around parent
        """
        for paren in [ ')+(', ')-(', ')/(', ')*(' ]:
            repl = paren.replace(')', ') ')
            repl = repl .replace('(', '( ')
            line = line.replace(paren,repl)
        return line
        



class Formatter:

    """class Formatter
    
    Provides pretty (and uniform) printing of Mortran source code.  We will impose some
    (harsh) rules on the format of the output files.  Sorry but they will become quite
    unreadable if people have complete freedom to format code the way they want.  The
    XML parser will insert blank lines seemingly at random, indentation levels will not
    be consistent between the AgML and moretran source, etc...  So we need to take steps
    to format nicely

    """

    margin_l = 0    # Mortran is semi-fixed format.... left margin is gone, right remains
    margin_r = 72
    
    def __init__(self,indent="   ",limit=80,level=2):
        self.level  = level
        self.indent = indent
        self.limit  = limit

    # ================================================
    #
    # Function which does the printing
    def __call__(self, line, indent=True, cchar=',', lstrip=True, debug=False, breakers=' ,' ):

        if ( debug ):
            print 'Pretty Print called with: '
            print line
            print ''
        

        if ( indent and lstrip ):
            line = line.lstrip()    # remove whitespace on left

        myline = ""                 # indent to proper level
        i=0
        if ( indent ):
            while (i<self.level):
                myline+=self.indent
                i+=1

        # Perform replacement of structure tags
        for search,replace in _structures.iteritems():            
            line = line.replace( search.lower(),
                                 replace.upper() )
        
        self.cprint (myline+line,cchar,debug,breakers)  # print the line

        
    def cprint(self,line,cchar,debug,        breakers = ' ,' ):
        """
        conditional print.  line is not printed if empty.
        cchar is(are) the continuation character(s) appended at the
        end of continuation lines
        """

        limit = self.limit

        if ( debug ): print 'cprint got '+ line
        
        # Require non null characters
        if ( not len(line.strip()) ): return

        # Split the line at whitespaec
        segments = line.split();
        nseg     = len(segments)

        # Create output line and pad it to the current
        # indent level
        oline = ''; i=0
        while ( i<self.level ):            oline+=self.indent;            i+=1
        
        tline = ''
        for iseg,seg in enumerate(segments):
            if ( len(tline) + len(seg) + 1 > limit ):
                oline += '%s %s\n'%(tline, cchar)
                tline = '\t'
                tline += '%s '%seg
            else:
                tline += '%s '%seg

        oline += tline

        print oline
        

    def increase(self,line):
        """
        increases or decreases the indentation level when certain key
        words are detected on the line
        """
        incs = ['MODULE','SUBROUTINE','DO','THEN','ELSE','BLOCK','FILL']
        for inc in incs:
            try:
                index = line.index(inc)
                #$$self.level += 1
            except ValueError: # substring not found
                pass

    def decrease(self,line):
        """
        increases or decreases the indentation level when certain key
        words are detected on the line
        """
        decs = ['END','ENDFILL','ENDBLOCK!','ENDDO']        
        for dec in decs:
            try:
                index = line.index(dec)
                #$$self.level -= 1
            except ValueError: # substring not found
                pass             




# ====================================================================================================
form = PrettyPrint()
#form = SimplePrint()
formatter = Formatter()


# ====================================================================================================
# Document syntax
class Document( Handler ):
    def __init__(self):
        self.parent = None
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p

# ====================================================================================================
# Begin definition of Mortran syntax


class Module ( Handler ):
    def __init__(self):
        self. name    = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self, tag, attr):
        name = attr.get('name','NONE')
        comm = attr.get('comment','NONE')
        self.name = name
        _in_module = True
        form ( "MODULE %s   %s" % ( name, comm ) )
        form ( '""" AGML variables """' )
        form ( 'REAL :: agml_rotm(9), agml_thetax, agml_phix, agml_thetay, agml_phiy, agml_thetaz, agml_phiz' )
        form ( 'COMMON /agml_vars/ agml_rotm, agml_thetax, agml_phix, agml_thetay, agml_phiy, agml_thetaz, agml_phiz' )
               
    def characters( self, content ):
        form( content, cchar=' _' )        
    def endElement(self, tag ):
        _in_module = False
        #form ( "Write (*,*) '===>> \t[End AgML Module %s]\t <<===';" % self.name )
        form ( "<W> '===>> \t[End AgML Module %s]\t <<==='; (A32,/,/);" %self.name )
        form ( "END! Module %s" % self.name )
    def addVar(self,name,type,dim,value,comment):
        if ( comment != None ):
            form( "%s %s ! %s"%( type, name, comment ) )
        else:
            form( "%s %s"%( type, name ) )


# ==================================================================================================== class Block ( Module ):
class Block( Handler ):
    def __init__(self):
        self. name = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Handler.__init__(self)    
    def setParent(self,p): self.parent = p
    def startElement(self, tag, attr):
        name = attr.get('name','NONE')
        comm = attr.get('comment','NONE')
        self.name = name
        _in_block = True
        form( '! ---------------------------------------------------------------------------------- %s'%name )
        form( "BLOCK %s   %s" % ( name, comm ) )
    def endElement(self, tag ):
        _in_block = False
        #form( "ENDBLOCK ! %s" % self.name )
        form( "ENDBLOCK" )        
    def characters( self, content ):

        # Go through the content and trim down on excess white
        # space w/in expressions
        for op in ['+','-','/','*','(',')']:

            size = len(content)
            content = content.replace(op+' ',op)
            content = content.replace(' '+op,op)            
            while size != len(content):
                size = len(content)
                content = content.replace(op+' ',op)
                content = content.replace(' '+op,op)

        # Now go back and expand around +, - so that
        # line breaks can be inserted properly
        for op in ['+','-']:
            size = len(content)
            content = (' '+op+' ').join( content.split(op) ) ################################################################################ MEH ####
        
        form( content, cchar=' _')#debug=True )

class Group( Handler ):
    def __init__(self):
        self. name=""
        self. comment=""
        Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        self.cond = attr.get('if',None)
        form( '! Reference system %s ignored'%self.name )


class Export ( Handler ):
    """
    Language-specific language block.  Code wrapped in an export block will only
    be output to the export file if the language attribute contains Mortran
    """
    def __init__(self):

        self.name      = "Mortran"
        self.form      = form
        self.formArray = formArray
        self.formatter = formatter
        
    def startElement(self, tag, attr):
        """
        Set export flag based on language specified in the export block
        """
        global form, formArray,formatter
        language = attr.get('language',None)
        
        if language:
            _isExporting = language.count(self.name)
            if _isExporting:
                form = self.form # Restore control to standard format output
                formArray = self.formArray
                formatter = self.formatter
            else:
                form      = NoPrint()
                formArray = NoPrint();
                formatter = NoPrint()
                
    def endElement(self,tag):
        """
        Restore export when exiting an export block
        """
        global form, formArray, formatter
        form      = self.form
        formArray = self.formArray
        formatter = self.formatter
                
    def characters( self, content ):
        form( content )
        

# ====================================================================================================
class Subroutine ( Handler ):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def addVar(self,name,type,dim,value,comment):        
        form( "%s %s ! dummy addVar function"    )

    def startElement(self,tag,attr):
        self.name = attr.get('name',None)        
        assert self.name
        form( '! ---------------------------------------------------------------------------------- %s'%self.name )        
        args = attr.get('args',None)
        if args:
            args = args.strip('[')
            args = args.strip(']')
            args = args.replace("'","")
            args = args.split(',')
        else:
            args = []
        self.args = args
        declaration = self.name        
        arglist = '('
        for arg in self.args:
            arglist += '%s,'%arg
        arglist=arglist .rstrip(',')
        arglist +=  ')'
        declaration += arglist
        form( "Subroutine %s"%declaration )                    

    def endElement(self, tag):
        form ( "End !Subroutine %s"%self.name )
        pass

    def characters( self, content ):
##      print "cHai! " + content
        content = content.strip() # remove whitespace
        if len(content):
            form(content)



class Assign(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p

    def startElement(self,tag,attr):
        var = attr.get('var')
        val = attr.get('value')
        form( '%s = %s' %(var,val))

class Keep(Handler):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        print "Warning in Mortran.Keep: Keep not implemented"


# ====================================================================================================
class Content ( Handler ):

    def setParent(self,p): self.parent = p    
    def __init__(self):
        self. content = False
        Handler.__init__(self)
    
    def characters( self, content ):
        if ( self.content ):
            form( content )
        else:
            if ( len(content.strip()) > 0 ):
                 form( "CONTENT " + content.lstrip() )
                 self.content = True


# ====================================================================================================        
class Include ( Handler ):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement( tag, attr ):
        file = attr.get('file',None)
        if ( file != None ):
            form( "#include \"%s\"" % file )


# ====================================================================================================
class Cde ( Handler ):
    cde = False
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        pass
    def characters(self, char):
        if ( len( char.strip() ) > 0 ):
            form( "+CDE,"+char.lower().strip()+"." )


# ====================================================================================================
# TODO: Setup inline to accept arguements and return values from embedded tags
class Inline( Handler ):

    arguements = []
    result     = None
    def __init__(self): 
        Handler.__init__(self)
        self.arguements = []
        self.result     = None
    def setParent(self,p): 
        self.parent = p

    def addArguement(self,arg):
        self.arguements.append(arg)

    def addResult(self,result):
        self.result = result
        
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        self.type = attr.get('type','real')
        if self.type in type_map.keys():
            self.type = type_map[ self.type ]

        
    def endElement(self,tag):
        #
        form('!* Lambda function %s'%self.name)
        # First declare the arguements
        arglist = ''

        for a in self.arguements:
            name = a.var
            type = a.typ
            arglist += name
            arglist += ','
            # If the variable is in the symbol table'
            # skip adding it
            try:
                lookup = _symbol_table[ name.lower() ]
            except KeyError:
                form ('%s %s ! arguement for %s'%(type,name,name))
        arglist = arglist.rstrip(',')
            
        # Next output the function, name and return value
        form( '%s %s'%(self.type,self.name) )
        form( '%s(%s) = %s'%(self.name, arglist, self.result) )

    def characters(self,content):
        pass


#  ====================================================================================================        
class External( Handler ):
    def __init__(self):
        Handler.__init__(self)
    def startElement(self,tag,attr):
        routine = attr.get('routine')
        form( 'EXTERNAL %s' % routine )
    def endElement(self,tag):
        pass

#  ====================================================================================================        
class Import( Handler ):
    def __init__(self): Handler.__init__(self)

    def startElement(self,tag,attr):
        map  = {'true' : True, 'false' : False }
        file = attr.get('file',None)
        stat = map[ attr.get('verbatim','false') ]
        assert(stat)

        # If the file does not exist, prepend $STAR to it
        try:
            test = open( file )
        except IOError:
            file = '$STAR/%s'%file

        if file[0]=='$': # Handle environment variable in 1st position
            temp = file.split('/')
            temp[0] = os.getenv(temp[0][1:])
            file = '/'.join(temp)

        with open(file,'r') as f:

            for line in f:
                if line.strip()=='\n':
                    continue
                print line.rstrip()

    def endElement(self,tag):
        pass
    

#  ====================================================================================================
class Comment( Handler ):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p
    def startElement(self,tag,attr):
        self.type=attr.get('type','!')
        self.comments = ''
    def characters(self,c):
        self.comments += c
    def endElement(self,tag):
        global export_comments
        if ( export_comments ):
            form( "%s %s"%( self.type, self.comments ), indent=False )
    
    


# ====================================================================================================
class Varlist( Handler ):
    def setParent(self,p):
        self.parent = p
    def __init__(self):
        self.mylist = []
        self.type   = None
        self.dim    = None
        self.len    = None
        Handler.__init__(self)        
    def startElement(self,tag,attr):
        type = attr.get('type',None)
        if ( type == None ):
            type = "real *4"
        if ( type == 'float' or type == 'Float_t' ):
            type = "real *4"
        if ( type == 'double' or type == 'Double_t' ):
            type = "real *8"
        dim = attr.get('dim', None); self.dim = dim
        len = attr.get('len', None); self.len = len
        if ( type == "char" or type == "Char_t" ):
            type ="character *%s"%len
        if ( type == "int" or type == "Int_t" ):
            type = "Integer *4"
        self.type = type
    def characters(self,content):
        global _symbol_table
        for var in content.split(','):
            if ( len(var.strip()) ):
                self.mylist.append(var)
                _symbol_table[var.lower()] = self.type    # add to the symbol table                
    def endElement(self,tag):
        varlist = self.type + ' ' + ( ','.join(self.mylist) ).lstrip()
        form(varlist)
        print ''

        # Handle variables defined in functions
#        if ( isinstance( self.parent, Function ) ):
#            pass
#
#        else:
#            for i,l in enumerate(self.mylist):
#                if ( i==0 ): form( "%s %s   <<<"%( self.type, l ) )
#                else:        form( "          %s   <<<"%l )
# ====================================================================================================
class Var ( Handler ):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.type    = None
        self.content = False
        Handler.__init__(self)                
    def startElement(self,tag,attr):
        #if ( _in_struct ):
        #pass
        name = attr.get('name')
        type = attr.get('type','float')
        #print 'Var: type='+type+' name='+name
        if ( type == 'float' ):
            type = 'real'
        if ( type == 'double' ):
            type = 'double precision'
        value = attr.get('value',None)
        comment = attr.get('comment',None)
        dim     = attr.get('dim',None)
        if ( name != None ):
            self.parent.addVar(name,type,dim,value,comment)
        self.type = type
    def characters(self,content):
        if ( not self.content ):
            form( "%s %s"%( self.type, content ) )
        else:
            form( content )
# ====================================================================================================
class __OLD__Data( Handler ):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        variables=attr.get('variables')
        values   =attr.get('values')
        myline = "DATA ";
        for v in variables:
            myline+="%s,"%v
        myline=myline.rstrip(',')
        myline += '/'
        for v in values:
            myline += "%s,"%v
        myline=myline.rstrip(',')
        myline += '/'            
        form( myline )

class Data( Handler ):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        variables = attr.get('variables')  # comma separated list of variables
        values    = attr.get('values')     # and values        
        myline = 'DATA ' + variables + '/' + values + '/'
        form( myline )
    
class Parameter( Handler ):
    """
    Class to represent PARAMETER (name=value) statements.  Limitation: only one
    name,value pair allowed per parameter statement.
    """
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p
    def startElement(self,tag,attr):
        global _symbol_table
        type = attr.get('type','int')
        if ( type == 'int' ):
            type = 'integer *4'
        if ( type == 'float' ):
            type = 'real *4'
        if ( type == 'double' ):
            type = 'real *8'
        name = attr.get('name')
        #try:
        #    lu = _symbol_table[ name.lower() ]
        #except KeyError:
        #    print _symbol_table.keys()
        #    _symbol_table[ name.lower() ] = type
        #    form ( type + ' ' + name )
        #if ( type != None ):                      # simplifies age --> agml conversion
        #    form( type + ' ' + name )
        value = attr.get('value')
        form( 'PARAMETER (%s = %s)'%(name,value) )
class Enum( Handler ):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
# ====================================================================================================
class Struct( Handler ):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self. name = 0
        self. var_list = []
        self. var_dict = {}        
        Handler.__init__(self)
    def addVar(self,name,type,dim,value,comment):
#   def addVar( self, type, name, dim=None ):
        if ( dim != None ):
            name+="(%s)"%dim # dimension the variable
        self.var_list.append( name )
        self.var_dict[ name ] = type
    def startElement( self, tag, attr ):
        name = attr.get('name',None)
        self.name = name
        # form( "Structure %s {" % self.name )
        # Add structure to the structure dictionary
        # This will allow us to search for and replace
        # the 'STCT:' pattern with 'STCT_' in the
        # output formatter
        _structures[ name+ _agml_sep ] = name+_struct_sep
    def endElement( self, tag ):
        output = "STRUCTURE %s { "% self.name
        for i,v in enumerate(self.var_list):
            term=','
            if ( i+1 == len(self.var_list) ):
                term=''
            type = self.var_dict[ v ]
            if ( type == 'float' ): type = 'real'
            if ( type == 'int' or type == 'char' ):
                output += type+" "+v+term #form( type+" "+v+term )
            else:
                output += v+term
                # output += "\n\t"
        #form( "}! %s" % self.name )
        output += "}"
        form ( output )
# ====================================================================================================
class ArrayFormatter:
    def __init__(self,limit=60,indent='    ',level=2 ):
        self.limit  = 65
        self.indent = indent
        self.level  = level

    def __call__(self,line,cchar=','):

        # Break down into var = array ! comment
        var,     myarray = line    . split('=', 1)
        myarray, comment = myarray . split('!', 1)

        n=len(myarray)

        rows  = myarray[2:n-2].split(';') # Split 2x2 arrays
        nrows = len(rows)

        # Loop over all values in all rows and find the largest
        # field for output 
        myfield = 0
        for row in rows:
            values = row.split(',')
            for v in values:
                v = v.strip() # strip whitespace
                if ( len(v) > myfield ): myfield = len(v)

        # We will eventually build up an output line which is
        # headed by the name of the variable equal to the array
        # expression.  We start by indenting the output line,
        # then add the array name, equals sign and open the array
        # assignment
        output = ''
        for i in range(0,self.level):
            output += self.indent
        output += '%s = { ' % var

        # Each line break will be padded so that the values will
        # line up from line to line
        pad = ''
        for c in output:
            pad += ' '

        # This is the amount of left padding which we have setup
        npad = len(pad)

        # And this will be the number of fields per line of output
        nfields = ( self.limit - npad ) / (myfield+3)

        # Counter for the number of fields
        ifield  = 0

        for irow,row in enumerate(rows):
            values = row.split(',')

            buffer = ''
            for value in values:

                value = value.strip() # strip whitespace
                
                buffer += (' %s, ' % value).rjust(myfield+3)
                ifield += 1
                
                if ( ifield % nfields == 0 ):
                    buffer +='\n'             # Add a newline after nfields have been accumulated
                    buffer += pad

            # End of a row.  If this is the last row, strip the trailing
            # comma and replace with a closing brace
            nbuffer  = len(buffer)            
            if ( irow == nrows-1 ):                
                buffer = buffer . rstrip()
                buffer = buffer . rstrip('\n')
                buffer = buffer . rstrip(',')
                output += buffer[:nbuffer-2] + '}'

            else:            
                output += buffer[:nbuffer-2] + ';\n'
                output += pad
                ifield = 0 # and reset the field counter because we got to a line break

        # And now add the comment
        output += ' ! %s' % comment

        print output

        
        

        


        
formArray = ArrayFormatter(indent='   ')

class Fill( Handler ):

    def setParent(self,p): self.parent = p    

    def __init__(self):
        self. name = 0
        self. comm = 0
        self. var_list = [] # list of variables
        self. val_list = [] # list of values
        self. com_list = [] # list of comments        
        Handler.__init__(self)

    def addVar(self,name,type,dim,value,comment):    

        self.var_list.append(name)
        self.val_list.append(value)
        self.com_list.append(comment)

    def startElement(self,tag,attr):
        name = attr.get('name',None);        self.name = name
        comm = attr.get('comment',None);     self.comment = comm
        form( "FILL %s\t\t! %s"%( name, comm ) )

    def _OLD_endElement(self,tag):
        for i,var in enumerate(self.var_list):
            val = self.val_list[i]
            com = self.com_list[i]            
            form( "%s = %s\t\t! %s"%( var, val, com ) )
        
        form( "ENDFILL")

    def endElement(self,tag):

        for i,var in enumerate(self.var_list):

            val = self.val_list[i]
            com = self.com_list[i]

            if ( val.lstrip()[0]=='{' ):             # dealing with either matrix or array
                formArray ( '%s = %s ! %s'%( var, val, com ) )
            else:
                form      ( '%s = %s ! %s'%( var, val, com ) )

        form('ENDFILL')
                
            

    def __NEW_OLD_endElement(self,tag):

        limit = 60

        for i,var in enumerate(self.var_list):

            val = self.val_list[i].strip()  # value(s)
            com = self.com_list[i]          # comment

            if ( val[0]=='{' ):             # dealing with either matrix or array

                n=len(val)
                myval=val[1:n-1]

                print myval

                mylines = myval.split(';')
                nlines = len(mylines)

                output = '%s = {' % var
                justfy = '      '
                output += justfy
                justfy += justfy
                
                for myline in mylines:

                    values = myline.split(',')
                    nv = len(values)

                    for i,v in enumerate(values):
                        v = v.strip()
                        if ( i!= nv-1 ):
                            output += '%s, '% v
                            if ( len(output)%limit == 0 ):
                                output+='\n'
                        else:
                            output += '%s'  % v

                    if ( nlines > 1 ):
                        output += ';\n'
                        output += justfy
    
                output += '} ! %s '% com

                for line in output.split('\n'):

                    line = line.strip(',')
                    #print line                    
                    #form(line)
                
                        
            else:
                form ( '%s=%s ! %s'%( var, val, com ) )
            

            



# ====================================================================================================
class Use(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):
        struct = attr.get('struct')
        selector = attr.get('select', None)
        value    = attr.get('value',  None)
        if ( selector == None ):
            form( "use %s" % struct )
        else:
            form( "use %s %s=%s"%( struct, selector, value ) )
# ====================================================================================================
class Material(Handler):
    def __init__(self):
        Handler.__init__(self)
        self.name = ""
        self.opts=[]
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        mylist = [ 'a', 'z', 'dens', 'absl', 'radl',
                   'ifield','isvol','fieldm','tmaxfd',
                   'epsil','stemax','deemax','stmin'
                   ] # and additional medium parameters
        for a in mylist:
            opt = attr.get(a,None)
            if opt:                
                opt = opt.replace('material::','%')
                opt = opt.replace(' ','')
            if ( opt != None ):
                self.opts.append( "%s=%s "%(a,opt) )
    def endElement(self,tag):
        myline="MATERIAL %s " % self.name
        for opt in self.opts:
            myline += opt
        form( myline )
            
class Medium(Handler):

    def __init__(self):
        self.name = 0
        self.opts = []        
        Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        mylist    = ['ifield','fieldm','tmaxfd','epsil','stemax','deemax','stmin','isvol']
        for a in mylist:
            opt = attr.get(a,None)
            if ( opt != None ):
                self.opts.append( "%s=%s "%(a,opt) )
    def endElement(self,tag):
        myline="MEDIUM %s " % self.name
        for opt in self.opts:
            myline += opt
        form( myline )
    
class Mixture(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.name = ""        
        self.opts = []
        self.comps = []        
        Handler.__init__(self)                    
    def addComponent(self, comp):
        self.comps.append(comp)
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        mylist = [ 'a', 'z', 'isvol', 'dens', 'nc' ]        
        for a in mylist:
            opt = attr.get(a,None)
            if ( opt != None ):
                self.opts.append( "%s=%s "%(a,opt) )
    def endElement(self,tag):
        for c in self.comps:
            form( "%s"% c )
        myline="MIXTURE %s " % self.name
        for opt in self.opts:
            myline += opt
        form( myline )

class Component(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement( self, tag, attr ):
        name = attr.get('name')
        a    = attr.get('A')
        if ( a==None ): a = attr.get('a')
        z    = attr.get('Z')
        if ( z==None ): z = attr.get('z')
        w    = attr.get('W')
        if ( w==None ): w = attr.get('w')    
        self.parent.addComponent( "COMPONENT %s A=%s Z=%s W=%s"%( name, a, z, w ) )
class Attribute(Handler):
    def __init__(self):
        self.style = []
        self.name=""
        Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):
        self.name = attr.get('for')
        self.cond = attr.get('cond')
        for a in _agstar_attribute_list:
            v = attr.get(a,None)
            if ( v != None ):
                self.style.append( "%s=%s"%( a, v ) )
    def endElement(self,tag):
        if self.cond:
            form ( 'if %s {'% self.cond )
        
        line = "ATTRIBUTE %s "% self.name
        for a in self.style:
            line += "%s "%a
        form( line )

        if self.cond:
            form ( '}')
        

# --


class Shape(Handler):    
    def __init__(self):
        Handler.__init__(self)
        self.arglist = []
        
    def setParent(self,p):
        self.parent = p    

    def startElement(self,tag,attr):
        mytype = attr.get('type',None).lower()

        if mytype=='bbox':
            mytype = 'box'
            
        args = Dyson.Utils.Shapes.arglist( mytype )
        for a in args:
            val = attr.get(a)

            if mytype=='hype':
                if a=='stin' or a=='stout': a='thet'
                if a=='rin': a='rmin'
                if a=='rout': a='rmax'
                
            if val != None:
                # strip all white space from val
                val = ''.join( val.split(' ') )
                self.arglist.append( "%s=%s"%(a,val) )

        output="SHAPE %s "%mytype
        for a in self.arglist:
            output += "%s "%a

        form( output,limit=45 )
        
                    
class Create(Handler):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p        
    def startElement(self,tag,attr):
        """
        CREATE operator can accept a large number of attributes.
        """
        block = attr.get('block')
        keys=[]
        for key,value in attr.iteritems():
            keys.append(key)
        output = 'CREATE %s' %( block )
        for key in keys:
            if key != 'block':
                output += ' %s=%s'%( key, attr.get(key) )
        form( output )

# pads the line with whitespace around specified characters
def inflate(line,chars='={}+-*/()'):
    output=""
    for c in line:
        if ( chars.find(c) > 0 ):
            output+=' '+c+' '
        else:
            output += c
    return output
        

class Position(Handler):

    def __init__(self):
        self.pos  = []
        self.into = None
        self.block = None
        #$$$        self.form  = Formatter()
        Handler.__init__(self)        

    def setParent(self,p): self.parent = p

    def startElement(self,tag,attr):

        self.into = attr.get('in',None)
        self.block = attr.get('block',None)
        mylist=['x','y','z',
                'alphax','alphay','alphaz','ort',
                'thetax','thetay','thetaz',
                'phix','phiy','phiz','ncopy',
                # and more directy geant commands which may
                # or may not be supported by agstar
                'phi1','phi2','phi3',
                'theta1','theta2','theta3'
                ]

        for key in mylist:
            att = attr.get(key,None)        
            if ( att != None ):
                self.pos.append( "%s=%s"%( key,att ) )
        only=attr.get('konly',None)

        if ( only != None ):
            only=only.strip("'")
            self.pos.append( "%s='%s'" %( "konly", only ) )

            
    def endElement(self,tag):

        output  = 'POSITION %s ' %self.block
        if ( self.into != None ):
            output +=" in %s"%self.into
        last=len(self.pos);
        for i,pos in enumerate(self.pos):
            pos=pos.strip('\n') # chomp
            pos=pos.strip(',')  # strip stray commas from arguements
            output += " %s"% pos

        #self.form( output, cchar=' _' )
        #form( output, cchar=' _' )
        #form( output )
        formatter( output, cchar=' _' )



class Create_and_Position(Position):
    def setParent(self,p):
        self.parent = p    
    def __init__(self):
        #self.pos  = []
        #self.into = None
        #self.block = None
        #self.form = Formatter(limit=90)
        Position.__init__(self)                
    def startElement(self,tag,attr):


        # Arrange shape arguements first
        for key in shape_params:
            val = attr.get(key,None)
            if ( val != None ):
                pos = "%s=%s"%(key,val);
                #print "Add pos=%s" %pos
                self.pos.append(pos)

        # And next the position arguements
        Position.startElement(self,tag,attr)                        
                
    def endElement(self,tag):
        output  = "CREATE and POSITION %s " %self.block
        if ( self.into != None ):
            output +=" in %s"%self.into
        for i,pos in enumerate(self.pos):
            output += " %s"% pos.strip(',')
        form( output             )

# ====================================================================================================
class Placement(Handler):
    def __init__(self):
        Handler.__init__(self)
        self.contents = []
        #self.form=Formatter()

    def setParent(self,p): self.parent=p

    def startElement(self,tag,attr):

        # Positional arguements
        self.block = attr.get('block')
        self.into  = attr.get('in',None)
        self.x     = attr.get('x',None)
        self.y     = attr.get('y',None)
        self.z     = attr.get('z',None)
        self.only  = attr.get('konly',None)
        self.copy  = attr.get('ncopy',None)
        self.cond  = attr.get('if',   None)

        if self.only:
            self.only  = self.only.strip("'")
            self.only  = "'%s'"%self.only

        self.attr  = attr

    def endElement(self,tag):


        block  = self.attr.pop('block')
        mother = self.attr.pop('in',None)
        copy   = self.attr.pop('ncopy',None)
        only   = self.attr.pop('konly',None)
        cond   = self.attr.pop('if', None)
        matrix = self.attr.pop('matrix', None)

        if cond: formatter( 'IF %s {'%cond )



        # Handle rotation matrix
        if matrix:
            formatter( '""" Handle the matrix """' )
            form     ( 'agml_rotm = %s'%matrix )
            form     ( 'Call GVmxga(agml_rotm,agml_thetax,agml_phix,agml_thetay,agml_phiy,agml_thetaz,agml_phiz)' )
            # push theta, phi onto attribute stack
            for key in ['thetax', 'phix', 'thetay', 'phiy', 'thetaz', 'phiz']:
                self.attr[key] = 'agml_%s'%key
        
        output = 'POSITION %s '% block.upper()        

        if ( mother != None ):
            output += 'in %s '% mother.upper()

        # Handle positional arguements
        for key in ['x','y','z']:
            val = self.attr.pop(key,None)
            if ( val != None ):
                output += '%s=%s '%( key, val )

        # Loop over all remaining keys
        for key in self.attr.keys():

            if key=="group": continue
            
            val=self.attr[key]
            output += '%s=%s '%( key, val )
    
        for c in self.contents:
            output += c.output()

        if ( copy != None ): output += 'ncopy=%s ' %( self.copy )
        if ( only != None ): output += 'konly=%s ' %( self.only )

##        self.form(output,cchar='_')
##        form(output,cchar='_')
        
        formatter( output, cchar='_' )


        if matrix: form( 'call gprotm(%irot)' )
        
        if cond: formatter( '}' )
        
    def add(self,thingy):
        self.contents.append(thingy)

class Translation(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        self.x=attr.get('x',None)
        self.y=attr.get('y',None)
        self.z=attr.get('z',None)
        self.parent.add(self)
    def output(self):
        o=''
        if ( self.x != None ):
            o+='x=%s '%( self.x )
        if ( self.y != None ):
            o+='y=%s '%( self.y )
        if ( self.z != None ):
            o+='z=%s '%( self.z )
        return o

class Rotation(Handler):

    def __init__(self):
        Handler.__init__(self)
        self.key    = None
        self.value  = None
        self.angles = None

    def output(self):
        out=''

#       if self.key=='matrix':
#           out='""" Got a matrix %s """' % self.value
#           return out
        
        if ( self.key != None ):
            out='%s=%s '%(self.key, self.value)
            return out

        if ( self.angles != None ):
            list=['thetax','phix','thetay','phiy','thetaz','phiz']
            for key in list:
                val = self.angles.get(key,None)
                if ( val != None ):
                    out +='%s=%s '%(key, val)
                    
        return out
            
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):


        matrix = attr.get('matrix',None)
        if matrix:
            self.key = 'matrix'
            self.value = matrix
            self.parent.add(self)


        list = ['alphax','alphay','alphaz','ort']
        for key in list:
            val = attr.get(key)
            if ( val != None ):
                self.key   = key
                self.value = val
                if key=='ort': self.value = val.replace("+","")                     
                self.parent.add(self)
                return

        self.angles = {}
        list = ['thetax','phix',
                'thetay','phiy',
                'thetaz','phiz']

        for key in list:
            val = attr.get(key,None)
            if ( val != None ):
                self.angles[key] = attr.get(key)
        
        self.parent.add(self)        


        
# ====================================================================================================
class For(Handler):
    def setParent(self,p): 
    	self.parent = p
    def __init__(self): 
    	Handler.__init__(self)                    
    	self.var = ''
    def startElement(self,tag,attr):
        variable = attr.get('var',None)
        myfrom   = attr.get('from',None)
        to       = attr.get('to',None)
        step     = attr.get('step','1')
        if ( step == '1' ):   form( "DO %s = %s, %s"    %(variable, myfrom, to      ) )
        else:                 form( "DO %s = %s, %s, %s"%(variable, myfrom, to, step) )            
        self.var = variable
    def endElement(self,tag):
        form( "END DO" )
    def characters(self,content):
        form( content, cchar=' _' )        
        

class While(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)
    def startElement(self,tag,attr):
        expr=attr.get('expr')
        form( 'Do WHILE ( %s )' % expr )
    def endElement(self,tag):
        form( 'End DO' )
    def characters(self,content):
        form( content, cchar=' _' )                

class Foreach(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)

class Call(Handler):
    def setParent(self,p): self.parent = p
    def __init__(self):Handler.__init__(self)
    def startElement(self,tag,attr):
        self.routine = attr.get('routine')
        self.expr    = attr.get('expr')
        form('Call %s( %s )' %( self.routine, self.expr ) )

# ====================================================================================================
class If(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Handler.__init__(self)
        self.expr=0
    def startElement(self,tag,attr):
        expr = attr.get('expr')
        self.expr=expr
        form( "IF ( %s ) THEN"% expr )
    def endElement(self,tag):
        form( "ENDIF" )
    def characters(self,content):        
        form( content, cchar=' _' )        
        
class Then(Handler):
    def setParent(self,p): self.parent = p
    def __init__(self): Handler.__init__(self)    
class Elif(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Handler.__init__(self)
    def startElement(self,tag,attr):
        expr = attr.get('expr')
        form( "ELSEIF ( %s ) THEN"% expr )
    def characters(self,content):
        form( content, cchar=' _' )
        

class Else(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)
    def startElement(self,tag,attr):
        form( "ELSE" )
    def characters(self,content):
        form( content, cchar=' _' )

class Return(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)                    
    def startElement(self,tag,attr):
        value = attr.get('value')

        # For inline functions, let the inline class handle it
        if self.parent.__class__ == Inline:
            self.parent.addResult(value)
            return
        
        if ( value != None ): form( "RETURN %s"%value )
        else:                 form( "RETURN" )

class Check(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        expr=attr.get('expr')
        if ( expr != None ):
            form( "CHECK %s" % expr )
# ====================================================================================================
class Author(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        name = attr.get('name')
        email = attr.get('email',None)
        if ( email != None ):
            form( "Author %s <%s>"%( name, email ) )
        else:
            form( "Author %s"%name )
# ====================================================================================================
class Created(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        date = attr.get('date')
        form( "Created %s"% date )
# ====================================================================================================        
class Translator(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        name = attr.get('name')
        email = attr.get('email',None)
        if ( email != None ):
            form( "!Translator %s <%s>"%( name, email ) )
        else:
            form( "!Translator %s"%name )
# ====================================================================================================            
#class Comment(Handler):
#    def __init__(self): Handler.__init__(self)
#    def setParent(self,p): self.parent = p        
#    pass
# ====================================================================================================
class Par(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        name = attr.get('name')
        val  = attr.get('value').strip()
        
        form( "Call GStPar(ag_imed, '%s', %s)"%( name, val ) )

# ====================================================================================================        
class Cut(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        name = attr.get('name')
        val  = attr.get('value')
        form( "Call GStPar(ag_imed, \'%s\',%s)"%( name, val ) )
# ====================================================================================================
# ===> Slated for deprecation <===
class Hits(Handler):

    def __init__(self):
        Handler.__init__(self)
        self.hit_list = []
        self.arg_list = []
        self.type = None
    
    def setParent(self,p): self.parent = p        
    def addHit(self,hit,arg):
        self.hit_list.append(hit)
        self.arg_list.append(arg)
    def startElement(self,tag,attr):        
        self.name = attr.get('for')
        self.type = attr.get('type')
        list = [ 'xx', 'yy', 'zz',
                 'pz', 'py', 'pz',
                 'cx', 'cy', 'cz',
                 'x',  'y',  'z',
                 'ptot',
                 'eta', 
                 'slen', 'tof', 'step',
                 'sleng', 'lptot',
                 'birk', 'eloss', 'elos',                 
                 'user', 'etsp']
        
        for hit in list:
            myhit = attr.get(hit,None)
            if ( myhit != None ):
                self.addHit( hit, myhit )
    def endElement(self,tag):
        declare = "HITS %s "%( self.name )
        if ( self.type != None ):
            declare += 'type=%s '% self.type
        for i,hit in enumerate(self.hit_list):
            arg = self.arg_list[i]
            declare += "%s:%s "%( hit, arg )
        form(declare,breakers=' ')


class Hit(Handler):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent=p
    def startElement(self,tag,attr):
        """
        Save attribute list and append self to instrumentation's
        hit list
        """
        self.attr = attr
        self.parent.addHit(self)
        
    def __str__(self):
        meas = self.attr.get('meas',None)
        nbit = self.attr.get('nbits', self.attr.get('bins', None) )
        opts = self.attr.get('opts',None)
        mmin = self.attr.get('min', None)
        mmax = self.attr.get('max', None)

        out = '%s:%s:'%(meas,str(nbit))
        if opts:
            out += '%s'%opts
        if mmin:
            out += '(%s,%s)'%(mmin,mmax)
            
        return out+' '
        
        

class Instrument(Handler):
    def __init__(self):
        Handler.__init__(self)
        self.hit_list = []
    def setParent(self,p): self.parent = p
    def addHit(self,hit):
        self.hit_list.append(hit)

    def startElement(self,tag,attr):
        self.block = attr.get('block', attr.get('volume', None))
    def endElement(self,tag):
        declare = 'HITS %s '%(self.block)
        for hit in self.hit_list:
            declare += str(hit)
        form(declare,breakers=' ')


        
    
# ====================================================================================================       
class Gsckov(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    pass
# ====================================================================================================
class Info(Handler):
    def __init__(self):
        self.level=0
        self.tag=None
        self.format=0
        self.args=[]
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p
    def startElement(self,tag,attr):
        self.tag = attr.get('tag',None)
        self.level = attr.get('level',0)
        self.format= attr.get('format')
    def characters(self,content):
        content=content.strip()
        for ele in content.split(','):
            self.args.append(ele)

    def descriptors(self):
        array  = self.format.split('{')
        mylist = ""

        stripLast = True

        # Loop over all elements
        for element in array:

            # If the element is a format descriptor
            if '}' in element:
                # Strip the right bracket
                element=element.strip('}')
                # Get the last character
                last = element[ len(element) - 1 ].upper()
                assert( last in "IDF" )
                # Get the rest
                rest = element[ 0 : len(element) - 1 ]
                # And append the descriptor to the list
                mylist += "', %s%s, '"%(last,rest)
                stripLast = True                

            # Otherwise append the element
            else:
                mylist += element
                stripLast = False

        if stripLast:
            mylist=mylist.strip(", '")


        return mylist
    
    def endElement(self,tag):
        output = '<W> '
        for i,a in enumerate(self.args):
            a = a.strip()
            if i and i<len(self.args): output += ' ,'
            output += '%s'%a
        output += ';'

        # Get the list of descriptors
        desc = self.descriptors()
        output += "  ('" + desc + ");"

        
        form( output )

        
        

    
            
    
# ====================================================================================================

class Print(Handler):
    def __init__(self):
        self.level  = 0
        self.format = 0
        self.args   = []        
        Handler.__init__(self)
    def setParent(self,p): self.parent = p

    def startElement(self,tag,attr):
        self.level  = attr.get('level','1')
        self.format = attr.get('fmt')

    def characters(self,content):
        if ( len(content.strip()) > 0 ):
            self.args.append(content)
    def __OLD__endElement(self,tag):
        if ( not len(self.args) ):
            self.args.append('')
        output = "Prin%s %s"%( self.level, self.args[0].strip() )
        for i,a in enumerate(self.args):
            if ( i and len(a.strip())>0 ):
                output += ", %s"%( a.strip() )
        output += ";\n"
        output += "\t(%s)" % self.format
        output += "\n"
        form( output )

    def endElement(self,tag):
        if ( not len(self.args) ):
            self.args.append('')
        output = "Prin%s %s"%( self.level, self.args[0].strip() )
        for i,a in enumerate(self.args):
            if ( i and len(a.strip())>0 ):
                output += ", %s"%( a.strip() )
        output += ";"
        form ( output )

        output  = ''
        output += "\t  (%s)" % self.format
        output += "\n"
        form( output )
        
# ====================================================================================================
class Replace(Handler):

    def __init__(self):
        self.match = ""
        self.replace = []
        Handler.__init__(self)

    def setParent(self,p):
        self.parent = p

    def startElement(self,tag,attr):
        match = attr.get('match')
        self.match = match

    def characters(self,content):

        if content.strip()!='\n':        
            self.replace.append(content)

    def endElement(self,tag):
        
        if ( len(self.replace)==1 ):
            form( "REPLACE [%s] with [%s];"%( self.match.rstrip(';'),
                                              self.replace[0].rstrip(';') ) );
        else:
            print ""
            form( "REPLACE [%s] with [" %self.match.rstrip(';') )
            for rpl in self.replace:
                rpl = rpl.rstrip(';')
                form( "    %s"%( rpl ) )
            form( "    ];" )
            print ""
        
# ====================================================================================================
class Function(Handler):

    def __init__(self):
        self.parent      = None
        self.name        = ""
        self.arg_list    = []
        self.arg_intent  = {}
        self.arg_type    = {}
        self.arg_default = {}
        
        Handler.__init__(self)
    def addArg(self, name, type, intent, default=None):
        self.arg_list.append(name)
        self.arg_type[ name ] = type
        self.arg_intent[ name ] = intent
        self.arg_default[ name ] = default
    def setParent(self,p):
        self.parent = p
    def startElement(self, tag, attr):
        self.name = attr.get('name',None)
        self.type = attr.get('type','void')
    def content(self, content):
        pass
    def endElement(self,tag):
        pass

class Arguement(Handler):
    def __init__(self):
        self.parent = None
        Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self, tag, attr):
        
        self.var = attr.get('name')
        self.typ = attr.get('type')
        self.intent = attr.get('intent')
        
        # Arguements in inline functions will be handled by the parent
        if self.parent.__class__ == Inline:
            self.parent.addArguement(self)
            return
        
        # Arguements of subroutines are 
        form( '%s %s !intent=%s'%(self.typ, self.var, self.intent) )
        
    def content(self, content):
        pass
    def endElement(self,tag):
        pass
# ====================================================================================================
class Fatal(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    pass # but an exception is coming!
# ====================================================================================================

if __name__ == '__main__':


    # Test of PrettyPrinter
    if ( 1 ):
        form = Formatter()
        TEST=" POSITION BPIP  x=toff_ElecHgt-3.*toff_RailThck/2.-toff_CoolOutR y=(tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR)"
        form(TEST)
                             
        
    

    if ( 0 ):

        s = Subroutine()
        s.characters("test")

    if ( 0 ):
        b = Block()
        attr = { 'name':'test', 'comment': 'this is a test' }
        b.startElement( 'Block', attr )

    if ( 0 ):
        c = Content()
        c.startElement( 'Content', {} )
        c.characters( 'AAAA,BBBB,CCCC,DDDD' )

    if ( 0 ):

        s = Struct()
        attr = { 'name' : 'HEXG' }
        s.startElement( 'Struct', attr )
        s.addVar('float','type')
        s.addVar('float','irad')
        s.addVar('float','clad')
        s.addVar('float','thick')
        s.addVar('float','xyz',3)
        s.endElement( 'Struct' )

        f = Fill()
        attr = { 'name' : 'HEXG', 'comment' : 'Small hex tile geometry' }
        f.startElement( 'Fill', attr )
        f.addVar('type','1','1=small tile 2=large tile')
        f.addVar('irad','4.23','inscribing circle radius')
        f.addVar('clad','1.20','cladding thickness')
        f.endElement('Fill')

        u = Use()
        attr = { 'struct' : 'HEXG', 'select': 'type', 'value' : '2' }
        u.startElement('Use',attr)
       
