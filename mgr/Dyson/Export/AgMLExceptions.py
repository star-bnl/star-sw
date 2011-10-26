from exceptions import Exception

from Dyson.Utils.Shapes  import listOfShapes


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
        return output

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
        return output

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
            
        return output

# ===============================================================================
class AgmlNameError(Exception):
    def __init__(self,volume,tag):
        self.volume = volume
        self.tag   = tag
    def __str__(self):
        output = """
        
        --> AgML Error line %i: %s must be 4 characters for backwards compatibility <--
        """%(self.locator.getLineNumber(),self.tag)

        return output
    
# ===============================================================================
class AgmlCommentError(Exception):
    def __init__(self,volume,tag):
        self.volume = volume
        self.tag   = tag
    def __str__(self):
        output = """
        
        --> AgML Error line %i: %s requires a comment field <--
        """%(self.locator.getLineNumber(),self.tag)
        
        return output
    

# ===============================================================================
class AgmlShapeError(Exception):
    def __init__(self,volume,shape):
        self.volume = volume
        self.shape  = shape

    def __str__(self):
        output = """
        
        --> AgML Error line %i: shape %s is unknown"%s" <--
        """%(self.locator.getLineNumber(),self.shape)

        return output        

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
        return output

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
        return output
# ===============================================================================
class AgmlFillMissingVarError(Exception):
    def __init__(self,struct,varname):
        self.struct = struct
        self.varname = varname
        Exception.__init__(self)
    def __str__(self):
        output = """

        --> AgML Error line %i: struct %s has no variable %s <--
        """%(self.struct,self.varname)
        return output
