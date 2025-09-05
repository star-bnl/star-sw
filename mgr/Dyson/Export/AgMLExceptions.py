import os
from exceptions import Exception


from Dyson.Utils.Shapes  import listOfShapes

if ( os.getenv('AGML_COLORED_EXCEPTIONS',False) ):
    def RED(x): return "\033[5m\033[31m"+x+"\033[0m"
    def YELLOW(x): return "\033[5m\033[33m"+x+"\033[0m"
    def BLUE(x): return "\033[5m\033[34m"+x+"\033[0m"
    def MAGENTA(x): return "\033[5m\035[34m"+x+"\033[0m"
else:
    def RED(x): return x
    def YELLOW(x): return x
    def BLUE(x): return x
    def MAGENTA(x): return x



# ===============================================================================
class ContentError(Exception):
    """
    Raised when a volume is not declared in content

    parameters:
    
       volume -- name of the missing volume
       module -- name of the module in which the error occurs
       
    """
    def __init__(self, volume, module):
        self.volume = volume
        self.module = module
    def __str__(self):
        output = """
        
============-=================================================
        
AgML Error: Volume %s not found in <Content> of Module %s
        
=============-================================================
        """ % (self.volume, self.module )
        return RED(output)

# ===============================================================================
class MissingError(Exception):
    """
    Raised when a volume is declared in content but is not
    implemented in the AgML file
    """
    def __init__(self,volume,module):
        self.volume = volume
        self.module = module
    def __str__(self):
        output = """
        
 ============-=================================================
 
 AgML Error: Volume %s declared in content is not implemented
             in module %s
 
 =============-================================================
        """ % (self.volume, self.module )
        return RED(output)

# ===============================================================================
class AgmlArrayError(Exception):
    def __init__(self,array,struct=None,reason=None):
        self.array = array
        self.struct = struct
        self.reason = reason
    def __str__(self):

        arrayName = ""
        if self.struct:
            arrayName += '%s.'%self.struct
        arrayName += self.array
        
        output = """
        
 ============-=================================================
 
 AgML Error: With array %s"""%arrayName
        if self.reason:
            output+="""
             %s"""%self.reason
            output+="""
 ============-=================================================
 """
            
        return RED(output)

# ===============================================================================
class AgmlNameError(Exception):
    def __init__(self,volume,tag):
        self.volume = volume
        self.tag   = tag
    def __str__(self):
        output = """
        
        --> AgML Error line %i: %s must be 4 characters for backwards compatibility <--
        """%(self.locator.getLineNumber(),self.tag)

        return RED(output)
    
# ===============================================================================
class AgmlCommentError(Exception):
    def __init__(self,volume,tag):
        self.volume = volume
        self.tag   = tag
    def __str__(self):
        output = """
        
        --> AgML Error line %i: %s requires a comment field <--
        """%(self.locator.getLineNumber(),self.tag)
        
        return RED(output)
    

# ===============================================================================
class AgmlShapeError(Exception):
    def __init__(self,volume,shape):
        self.volume = volume
        self.shape  = shape

    def __str__(self):
        output = """
        
        --> AgML Error line %i: shape %s is unknown"%s" <--
        """%(self.locator.getLineNumber(),self.shape)

        return RED(output)

# ===============================================================================
class AgmlAttributeWarning(Warning):

    def __init__(self,volume,tag,key,value):
        self.volume = volume 
        self.tag    = tag        
        self.key    = key
        self.value  = value
    def __str__(self):
        output = """

        --> AgML Warning line %i: %s has invalid attribute %s="%s" <--
        """%(self.locator.getLineNumber(),self.tag,self.key,self.value)
        return YELLOW(output)

# ===============================================================================
class AgmlMissingAttributeWarning(Warning):

    def __init__(self,volume,tag,key):
        self.volume = volume 
        self.tag    = tag        
        self.key    = key

    def __str__(self):
        output = """
        
        --> AgML Warning line %i: %s missinge required attribute %s <--
        """%(self.locator.getLineNumber(),self.tag,self.key)
        return YELLOW(output)
# ===============================================================================
class AgmlFillMissingVarError(Exception):
    def __init__(self,struct,varname):
        self.struct = struct
        self.varname = varname
        Exception.__init__(self)
    def __str__(self):
        output = """

        --> AgML Error line %i: struct %s has no variable %s <--
        """%(self.locator.getLineNumber(),self.struct,self.varname)
        return RED(output)


# ===============================================================================
class MixtureComponentError(Exception):
    def __init__(self,mixture):
        self.mixture = mixture
        Exception.__init__(self)
    def __str__(self):
        output = """

        --> AgML Error line %i : Mixture %s has invalid weights for components. <--

            All weights must be < 1 for a mixture by weight
                (and sum of all weights = 1)
                
            All weights must be integer for a mixture by chemical formula                

            """%(self.locator.getLineNumber(), self.mixture.name)

        for ele in self.mixture.elements:
            output+= """
            Component %s a=%s z=%s w=%s"""%(ele['name'],ele['a'],ele['z'],ele['w'])
        output += "\n"

        return RED(output)


