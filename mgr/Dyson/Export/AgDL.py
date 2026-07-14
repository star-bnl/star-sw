from   Handler import Handler
import Dyson.Utils.Shapes

export_comments = False

_agstar_attribute_list = ['seen','colo','ltyp','serial','fill','lsty','lwid'];
_structures = {}
_struct_sep = '_'
_agml_sep = ':'
_soft_limit = 80 # applies a soft limit on the line length
_depth = 0
_prepend = '    '

# locator object which implements getLineNumber() and possibly other methods
# to be set by the syntax handler when the language is defined
locator = None

class Sanitize:

    """
    Helper function to strip illegal characters (in XML) from
    the output stream and replace them with their XML equivalent.
    """
    def __init__(self):
        self.table = {       # Replacement table
#           '>' : '&gt;',
#           '<' : '&lt;',
#           '&' : '&amp;'
            }
    def __call__(self,line):
        out = ''
        for c in line:
            try:
                out += self.table[c]
            except KeyError:
                out += c
        return out

sanitize = Sanitize()
            

class Operator(Handler):

    def __init__(self,firstKey=None,keylist=[]):        

        self.firstKey = firstKey
        self.keylist  = keylist
#       print firstKey

    def startElement(self,tag,attr):
        out = ''
        if ( self.firstKey != None ):
            try:
                firstValue = attr.pop(self.firstKey,None)
                if firstValue != None:
                    out += '%s '%(firstValue)
            except KeyError:
                pass
        for key in self.keylist:
            try:
                value = attr.pop(key)
                if value != None: out += '%s=%s '%( key, value.strip() )
            except KeyError:
                pass
        for key,value in attr.iteritems():
            if value != None: out += '%s=%s '%( key, value.strip() )
        out = sanitize(out)
        out = '%s '%( tag.upper() ) + out + ';'
        form( out )

class Container(Handler):

    def __init__(self,firstKey=None,keylist=[]):
        self.firstKey = firstKey
        self.keylist  = keylist

    def startElement(self,tag,attr):
        global _depth
        out = ''
#       if ( tag=='Placement' ):
#           print "PLACEMENT: 1st=%s"%self.firstKey        
        if ( self.firstKey != None ):
            try:
                firstValue = attr.pop(self.firstKey)
                if firstValue != None:
                    #out += '%s="%s" '%(self.firstKey,firstValue)
                    out += '%s '%(firstValue)
            except KeyError:
                pass
        for key in self.keylist:
            try:
                value = attr.pop(key,None)
                if ( value != None ): out += '%s=%s '%( key, value.strip() )
            except KeyError:
                pass
        for key,value in attr.iteritems():
            if value != None: out += '%s=%s '%( key, value.strip() )
        out = '%s '%tag.upper() + sanitize(out) + '{'

        form( out )

        if tag in ['Then','Else','Elif']:
            pass
        else:        
            _depth += 1
        
    def characters(self,contents):
        global _depth
        #_depth+=1
        contents = sanitize(contents)
        if ( len(contents.strip()) > 0 ):
            form( contents.lstrip() + ';' )
        #_depth-=1
    def endElement(self,tag):
        global _depth

        if tag in ['Then','Else','Elif']:
            pass
        else:        
            _depth -= 1
            
        form( '}; # %s' % tag )
        form( '' )


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
    def __call__(self,line):
        global _depth, _prepend        
        output = ''
        i=0
        while ( i<_depth ):
            output += _prepend
            i+=1
        output += line
        print output


# ====================================================================================================
#form = PrettyPrint()
form = SimplePrint()


# ====================================================================================================
# Document syntax
class Document( Container ):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p

    def startElement(self,tag,attr):
        pass
    def endElement(self,tag):
        pass


# ====================================================================================================

def sanitizeComments( line ):
    """
    Replaces all occurances of '--' with '++' in comments
    """
    length = len(line)
    output = ''
    i=0
    while i<length:
        c=line[i]
        if ( c == '{' or c == '}' ):
            c = ' '
            
        if ( i+1 < length ):
            d = line[i+1]
            if c+d == '--':
                output += '++'
                i=i+1
            else:
                output += c
        i+=1

    return output
                    
class Comment( Handler ): 
    def __init__(self):
        Handler.__init__(self)
        self.content = ''
    def setParent(self,p):
        self.parent = p
    def startElement(self,tag,attr):
        pass
    def characters(self,content):
        self.content = content
    def endElement(self,tag):
        global export_comments
        if ( export_comments ):
            content = sanitizeComments(self.content)            
            form ( '<!-- ' + content + ' -->' )
     
class Module ( Container ):
    def __init__(self):
        self. name    = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Container.__init__(self,firstKey='name')
    def setParent(self,p): self.parent = p
class Block ( Container ):
    def __init__(self):
        self. name = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Container.__init__(self,firstKey='name')    
    def setParent(self,p): self.parent = p

class Subroutine ( Container ):
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p

class Assign( Operator ):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p

class Keep( Operator ):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p    
    

class Content ( Container ):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self. content = False
        Container.__init__(self)

class Include ( Operator ):
    def __init__(self):
        Operator.__init__(self)
    def setParent(self,p): self.parent = p    

class Cde ( Container ):
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p    

class Inline( Operator ):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): 
    	self.parent = p
class External( Operator ):
    def __init__(self):
        Operator.__init__(self)

class Varlist( Container ):
    def setParent(self,p):
        self.parent = p
    def __init__(self):
        Container.__init__(self,firstKey='type')
#    def startElement(self,tag,attr):
#        # Need to strip out the 'list' from the attr
        

class Var ( Operator ):
    def setParent(self,p): self.parent = p    
    def __init__(self):
#       self.type    = None
#       self.content = False
        Operator.__init__(self,firstKey='name',keylist=['value','type','comment'])                

class Data( Operator ):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p
class Parameter( Operator ):
    """
    Class to represent PARAMETER (name=value) statements.  Limitation: only one
    name,value pair allowed per parameter statement.
    """
    def __init__(self):
        Operator.__init__(self,firstKey='name')
    def setParent(self,p):
        self.parent = p
class Enum( Operator ):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p    
# ====================================================================================================
class Struct( Container ):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,firstKey='name')
class Fill( Container ):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,firstKey='name',keylist=['comment'])
class Use(Operator):
    def __init__(self): Operator.__init__(self,keylist=['struct','select','value'])
    def setParent(self,p): self.parent = p    
class Material(Operator):
    def __init__(self):
        Operator.__init__(self,firstKey='name')
        self.name = ""
        self.opts=[]
    def setParent(self,p): self.parent = p    
class Medium(Operator):
    def __init__(self):
        self.name = 0
        self.opts = []        
        Operator.__init__(self,firstKey='name')
    def setParent(self,p): self.parent = p    
class Mixture(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.name = ""        
        self.opts = []
        self.comps = []        
        Container.__init__(self,firstKey='name')                    
class Component(Operator):
    def __init__(self): Operator.__init__(self,firstKey='name')
    def setParent(self,p): self.parent = p    
class Attribute(Operator):
    def __init__(self):
        self.style = []
        self.name=""
        Operator.__init__(self,firstKey='for')
    def setParent(self,p): self.parent = p    
class Shape(Operator):    
    def __init__(self):
        Operator.__init__(self,firstKey='type')
        self.arglist = []
    def setParent(self,p):
        self.parent = p    
class Create(Operator):
    def __init__(self):
        Operator.__init__(self,firstKey='block')
    def setParent(self,p):
        self.parent = p        
class Position(Operator):
    def __init__(self):
        self.pos  = []
        self.into = None
        self.block = None
        Operator.__init__(self,firstKey='block',keylist=['in','x','y','z'])        
    def setParent(self,p): self.parent = p
class Create_and_Position(Position):
    def setParent(self,p):
        self.parent = p    
    def __init__(self):
        self.pos  = []
        self.into = None
        self.block = None
        Position.__init__(self,firstKey='block',keylist=['x','y','z'])

class Placement(Container):
    def __init__(self):
        Container.__init__(self, firstKey='block',keylist=[] )
    def setParent(self,p):
        self.parent=p
        
class Translation(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent=p

class Rotation(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent=p



class For(Container):
    def setParent(self,p): 
    	self.parent = p
    def __init__(self): 
    	Container.__init__(self,firstKey='var',keylist=['from','to'])                    
    	self.var = ''
class While(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self): Container.__init__(self,firstKey='expr')                    
class Foreach(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self): Container.__init__(self)
class Call(Operator):
    def setParent(self,p): self.parent = p
    def __init__(self):Operator.__init__(self)
# ====================================================================================================
class If(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,firstKey='expr')
        self.expr=''
class Then(Container):
    def setParent(self,p): self.parent = p
    def __init__(self): Container.__init__(self)    
class Elif(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Container.__init__(self,firstKey='expr')
class Else(Container):
    def setParent(self,p): self.parent = p    
    def __init__(self): Container.__init__(self)
class Return(Operator):
    def setParent(self,p): self.parent = p    
    def __init__(self): Operator.__init__(self)                    
class Check(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p
class Author(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Created(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Translator(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Par(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
class Cut(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        

class Hits(Operator):
    def __init__(self):
        Operator.__init__(self,firstKey='for')
        self.hit_list = []
        self.arg_list = []
    def setParent(self,p): self.parent = p        
class Gsckov(Operator):
    def __init__(self): Operator.__init__(self)
    def setParent(self,p): self.parent = p        
    pass
class Print(Container):
    def __init__(self):
        self.level  = 0
        self.format = 0
        self.args   = []        
        Container.__init__(self,firstKey='level')
    def setParent(self,p): self.parent = p        
class Replace(Container):
    def __init__(self):
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p







class Function(Container):
    def __init__(self):
        self.parent      = None
        self.name        = ""
        self.arg_list    = []
        self.arg_intent  = {}
        self.arg_type    = {}
        self.arg_default = {}        
        Container.__init__(self)
    def setParent(self,p):
        self.parent = p
class Arguement(Container):
    def __init__(self):
        self.parent = None
        Container.__init__(self)
    def setParent(self,p): self.parent = p
class Fatal(Container):
    def __init__(self): Container.__init__(self)
    def setParent(self,p): self.parent = p        

if __name__ == '__main__':

    if ( 0 ):

        s = Subroutine()
        s.characters("test")

