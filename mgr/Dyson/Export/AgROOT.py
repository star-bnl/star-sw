from   Handler import Handler

import Dyson.Utils.Shapes
from   Dyson.Utils.Shapes    import shape_params
from   Dyson.Utils.IOHandler import IOHandler, IOBuffer, setExport

import os
import re

# Exception handling
from AgMLExceptions import ContentError, MissingError, AgmlArrayError, AgmlNameError, AgmlCommentError, AgmlShapeError, AgmlAttributeWarning, AgmlFillMissingVarError, MixtureComponentError, AgmlMissingAttributeWarning

#enable_warnings = os.getenv('AGML_WARNINGS',False)
#enable_warnings = ( os.getenv('STAR','...adev').find('adev') < 0 )
enable_warnings = True
from warnings import warn

from pyparsing  import *

oldplacement = False

banner = """
/*
 ******************************************************************************
 ******************************************************************************
 **                                                                          **
 ** This is generated code.  Do not alter.  You should instead edit the XML  **
 ** module which corresponds to your detector.  This code will be replaced   **
 ** on the next compilation.                                                 **
 **                                                                          **
 ******************************************************************************
 ******************************************************************************
 */
"""


# Legacy modules have errors in their definitions which must be preserved
# as a reference for past data sets.  We do not want to issue compile-time
# warnings for these modules.
legacy = [    "StarVMC/Geometry/BbcmGeo/BbcmGeo.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo1.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo2.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo3.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo4.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo5.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo6.xml",
              "StarVMC/Geometry/BtofGeo/BtofGeo7.xml",
              "StarVMC/Geometry/CalbGeo/CalbGeo1.xml",
              "StarVMC/Geometry/CalbGeo/CalbGeo2.xml",
              "StarVMC/Geometry/CalbGeo/CalbGeo.xml",
              "StarVMC/Geometry/CaveGeo/CaveGeo.xml",
              "StarVMC/Geometry/Compat/calbpar.xml",
              "StarVMC/Geometry/Compat/dummgeo.xml",
              "StarVMC/Geometry/Compat/etsphit.xml",
              "StarVMC/Geometry/Compat/ffpdstep.xml",
              "StarVMC/Geometry/Compat/fgtdgeo.xml",
              "StarVMC/Geometry/Compat/fhcmgeo.xml",
              "StarVMC/Geometry/Compat/fpdmgeo.xml",
              "StarVMC/Geometry/Compat/fstdgeo.xml",
              "StarVMC/Geometry/Compat/gembgeo.xml",
              "StarVMC/Geometry/Compat/hpdtgeo.xml",
              "StarVMC/Geometry/Compat/igtdgeo.xml",
              "StarVMC/Geometry/Compat/istbego.xml",
              "StarVMC/Geometry/Compat/itspgeo.xml",
              "StarVMC/Geometry/Compat/pixlgeo.xml",
              "StarVMC/Geometry/Compat/richgeo.xml",
              "StarVMC/Geometry/Compat/tpcegeo3.xml",
              "StarVMC/Geometry/Compat/tpcegeo.xml",
              "StarVMC/Geometry/Compat/wallgeo.xml",
              "StarVMC/Geometry/Compat/xgeometry.xml",
              "StarVMC/Geometry/EcalGeo/EcalGeo6.xml",
              "StarVMC/Geometry/EcalGeo/EcalGeo.xml",
              "StarVMC/Geometry/FpdmGeo/FpdmGeo1.xml",
              "StarVMC/Geometry/FpdmGeo/FpdmGeo2.xml",
              "StarVMC/Geometry/FpdmGeo/FpdmGeo3.xml",
              "StarVMC/Geometry/FsceGeo/FsceGeo.xml",
              "StarVMC/Geometry/FtpcGeo/FtpcGeo1.xml",
              "StarVMC/Geometry/FtpcGeo/FtpcGeo.xml",
              "StarVMC/Geometry/FtroGeo/FtroGeo.xml",
              "StarVMC/Geometry/MagpGeo/MagpGeo.xml",
              "StarVMC/Geometry/MutdGeo/MutdGeo2.xml",
              "StarVMC/Geometry/MutdGeo/MutdGeo3.xml",
              "StarVMC/Geometry/MutdGeo/MutdGeo4.xml",
              "StarVMC/Geometry/MutdGeo/MutdGeo.xml",
              "StarVMC/Geometry/PhmdGeo/PhmdGeo.xml",
              "StarVMC/Geometry/PipeGeo/PipeGeo00.xml",
              "StarVMC/Geometry/PipeGeo/PipeGeo.xml",
              "StarVMC/Geometry/PixlGeo/PixlGeo3.xml",
              "StarVMC/Geometry/QuadGeo/QuadGeo.xml",
              "StarVMC/Geometry/SconGeo/SconGeo.xml",
              "StarVMC/Geometry/ShldGeo/ShldGeo.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo1.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo2.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo3.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo4.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo5.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo6.xml",
              "StarVMC/Geometry/SisdGeo/SisdGeo.xml",
              "StarVMC/Geometry/SupoGeo/SupoGeo1.xml",
              "StarVMC/Geometry/SupoGeo/SupoGeo.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo10.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo11.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo1.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo2.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo3.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo4.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo5.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo6.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo7.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo9.xml",
              "StarVMC/Geometry/SvttGeo/SvttGeo.xml",
              "StarVMC/Geometry/TestGeo/TestGeo1.xml",
              "StarVMC/Geometry/TpceGeo/TpceGeo1.xml",
              "StarVMC/Geometry/TpceGeo/TpceGeo2.xml",
              "StarVMC/Geometry/UpstGeo/UpstGeo.xml",
              "StarVMC/Geometry/VpddGeo/VpddGeo1.xml",
              "StarVMC/Geometry/VpddGeo/VpddGeo2.xml",
              "StarVMC/Geometry/VpddGeo/VpddGeo.xml",
              "StarVMC/Geometry/ZcalGeo/ZcalGeo.xml"
              ]



# Control Flags
# ==========================================================
namespace       = True  # Classes go into namespaces
export_comments = True  # Comments will be exported

# Skip exporting of G3 routines
routine_skip_list = ['gsckov','gstpar']

# This list might not be used anywhere
iostack = []

# List of attributes allowed in the <Attribute ... /> operator
_agstar_attribute_list = [
    'seen',     # The family of volumes will be seen
    'colo',     # Defines the color of the volumes
    'ltyp',     # Defines line style
    'serial',   # If serial changes, forces the creation of a new volume
    'fill',     # G3 does something
    'lsty',     # Also line style
    'lwid',     # Line width
    'trans'     # Transparency in OpenGL/ROOT
    ];

_structures = {}

_struct_sep = '_'
_agml_sep = ':'

_soft_limit = 80 # applies a soft limit on the line length
_depth = 0
_prepend = ''

# TODO: Populate the symbol table on a Varlist
_symbol_table = {}
_struct_table = {} # values of structure members
_struct_dims  = {} # 



seperator = '// ---------------------------------------------------------------------------------------------------'
skip      = '//'

agml_functions = ['agexist']

output = IOHandler(newline=True)

current_block = "None"

# locator object which implements getLineNumber() and possibly other methods
# to be set by the syntax handler when the language is defined
locator = None

# Wrappers around the warning and exception handling in order to add the
# locator to the exception
def RaiseWarning( exception, warning=True ):

    #enable_warnings = ( os.getenv('STAR','...adev').find('adev') < 0 )    
    if not enable_warnings:
        return
    
    exception.locator = locator
    exception.file    = document.input
    if warning:
        warn(exception)
    else:
        raise exception
    
def RaiseException( exception ):
    exception.locator = locator
    exception.file    = document.input   
    raise exception


###############################################################################

def doxygenize( comment ): return '/// %s'%comment

def camelCase( text, places = [0] ):
    line = ""
    for i,c in enumerate(text):
        if i in places: line += c.upper()
        else:           line += c.lower()
    return line


def checkAttributes( tag, attr, mylist, skip=[], warning=True ):
    """
    Verify that the attribute list contains keys which are limited
    to the expected values provided in mylist.  Keys provided in
    the except list will not be checked.
    """
    for key,value in attr.iteritems():
        if key in skip: continue
        if key.lower() in skip: continue
        key = key.lower()
        if key in mylist:
            pass
        else:
            RaiseWarning( AgmlAttributeWarning(current_block, tag, key, value), warning )

def requireAttributes( tag, attr, mylist, warning=True ):
    """
    Require that all keys in mylist appears in the attribute list
    """
    for key in mylist:
        value = attr.get(key, None)
        if value==None:            
            RaiseWarning( AgmlAttributeWarning(current_block, tag, key, warning ) )

def requireAnyAttributes( tag, attr, mylist, warning=True ):
    """
    Require that at least one keys in mylist appears in the attribute list
    """
    
    for key in mylist:
        if key in attr.keys():
            return

    RaiseWarning( AgmlMissingAttributeWarning(current_block, tag, mylist ) )


# ----------------------------------------------------------------------------------------------------
def parseArray(array):
    """
    Given a string which is interpreted as an array, i.e.

    array="{1,2,3;4,5,6;7,8,9;}"   ! 2D array
    array="{1,2,3}"                ! 1D array

    returns a (nested) list representing the (2D) 1D array
    
    """

    work = array.strip()
    work = work.lstrip('{')
    work = work.rstrip('}')
    work = work.rstrip(';')
    work = work.replace(' ','')
    work = work.replace('\n','')
    
    out = []
    nline = 0
    for line in work.split(';'):
        out.append( list(line.split(',')) )
        nline = nline + 1

    if nline==1: out = out[0]
    return list(out)

# ----------------------------------------------------------------------------------------------------
def replacements( line ):
    global _struct_dims

    if line==None: return line

    # Work on a copy... meh
    myline = line

    #
    # Replace Fortran logical operators first (stray "." will kill in next step)
    #
    opers = [ '|', '&', '.or.', '.and.', '.not.', '.ne.', '.eq.', '.gt.', '.lt.','.leq.','.geq.', '.ge.', '.le.' ]
    table = { '.and.': '&&',
              '.or.' : '||',
              '.not.': '!',
              '.ne.' : '!=',
              '.eq.' : '==',
              '.gt.' : '>',
              '.lt.' : '<',
              '.leq.': '<=',
              '.geq.': '>=',
              '.ge.' : '>=',
              '.le.' : '<=',
              '|'    : '.or.',
              '&'    : '.and.',
              }
    for key in opers:
        value  = table[key]
        myline = myline.replace( key, value )

    #
    # Replace **N exponentiation with the ^Power(N) hack in the AgmlLib
    #    for N=2, 3
    myline = myline.replace("**2.0","*AgPower<Int_t>(2)")
    myline = myline.replace("**3.0","*AgPower<Int_t>(3)");    
    myline = myline.replace("**2.","*AgPower<Int_t>(2)")
    myline = myline.replace("**3.","*AgPower<Int_t>(3)");
    myline = myline.replace("**2","*AgPower<Int_t>(2)")
    myline = myline.replace("**3","*AgPower<Int_t>(3)");
    


    #
    # Replace all occurences of structures wich are followed by an "_" with
    # the name of the structure followed by  "."... so long as the structure
    # name is not preceded by either an "_" or a "." i.e.
    #
    # A_B_C --> A.B_C
    # B_C_D --> B.C_D
    # B_E   --> B.E
    # etc...
    #
    for struct in document.structs:

        # Lower case struct to be certain
        struct = struct.lower()

        # Find all occurences of the structure name which are followed
        # by but not preceded by an "_"

        if ( struct in myline.lower() ): # if the line doesn't contain the struct... no need to finditer

            for match in re.finditer( "(^|[^_.])%s_"%struct, myline, re.IGNORECASE ):

                Old = match.group()            # This is a non _ followed by STRUCT_
                New = Old.replace("_",".")     # This is a non _ followed by STRUCT.

                # This should strip out STRUCT_ and replace with STRUCT.
                # without changing _STRUCT_
                myline = myline.replace( Old, New )     
                                                    

    # NEW STRUCT IMPLEMENTATION
        
    # Now we need to change the indexing of the arrays in the structure.  We
    # scan for something matching struct.var( idx ) and replace with struct.var[idx-1]

    myline = myline.replace(' ','') # Remove ALL whitespace


    for struct in document.structs:

#        if struct in myline:
#            print "----------------------------------------------------------"                        
#            print ":in:  %s"%myline
        
        # Lower case struct to be certain
        struct = struct.lower()        

        # The loop below searches for (I) and replaces with [I-1]
        # We replace all , with () so that the replacement will replace (I,J,...) with [I-1][J-1][...-1]
        # myline = myline.replace(',',')(')

        # Loop over all variables in this struct.  
        for key,value in _struct_dims.iteritems(): # isnt this backward?
            
            if ( key not in myline.lower() ): continue # can skip finditer

            #
            # Look for things of the form struct.array(index,jndex) and replace with struct.array[int(jndex)-1][int(index)-1]
            #
            expr = "%s\(([a-z0-9_\+\-\*\/\ \.]*)\,([a-z0-9_\+\-\*\/\ \.]*)\)"%key
            for match in re.finditer( expr, myline, re.IGNORECASE ):
                Old = match.group()

                length = Old.replace(key,'')
                length = length.strip('(')
                length = length.strip(')')                
                length = length.split(',')
                index = ""
                for i in reversed(length):
                    index += '[int(%s)-1]'%i

                New = key + index
                
                # Old is goingt to be of the form struc.array(I,J).  We need to
                # revese this to struct.array[J-1][I-1]

                #New = Old.replace( ',', ')(' )
                #New = New.replace('(','[int(')
                #New = New.replace(')',')-1]')                
                
                myline = myline.replace(Old,New)

            #
            # Look for things of the form struct.array(index) and replace with struct.array[int(index)-1]
            #
            expr = "%s\(([a-z0-9_\+\-\*\/\ \.]*)\)"%key

            for match in re.finditer( "%s\(([a-z0-9_\+\-\*\/\ \.]*)\)"%key, myline, re.IGNORECASE  ):

                Old = match.group()
                New = Old.replace('(','[int(')
                New = New.replace(')',')-1]')

                #print match
                #print New

                myline = myline.replace(Old,New)



#        if struct in myline:
#            print ":out: %s"%myline                
            
            




            
    #
    # Replace single quote for double quote
    #
    temp = ""
    for c in myline:
        if c=="'": c='"'
        temp += c
    myline=temp
    
    # Replace 'ONLY' and 'MANY' with kOnly and kMany
    if oldplacement:
        myline =    myline.replace("ONLY", 'AgPlacement::kOnly')
        myline =    myline.replace("MANY", 'AgPlacement::kMany')
    else:
        myline =    myline.replace("ONLY", 'AgPosition::kOnly')
        myline =    myline.replace("MANY", 'AgPosition::kMany')
   
    # System of units replacement
    myline =    myline.replace("keV","*1.0E-6")
    myline =    myline.replace("MeV","*0.001")
    myline =    myline.replace("GeV","*1.0") 
   
    return myline
def arrayInitializer( line ):
    pass

class NotYet( Exception ):
    def __init__(self):
        pass
    def __str__(self):
        return repr(self)+": Not yet implemented"

# Document syntax
document = 0
current  = 0

class Document( Handler ):

    def __init__(self):
        global document
        self.parent = None
        Handler.__init__(self)
        document = self

        self.blocks     = []
        self.functions  = []
        self.variables  = []
        self.structs    = []
        self.fills      = []        
        #self.fill_count = {}
        self.content    = []
        self.geometries = []

    def setParent(self,p):
        self.parent = p

    def startElement(self, tag, attr):
        global enable_warnings
        
        self.options  = attr.get('cmdline',None)   # The command line options
        self.input    = attr.get('file')           # The complete path to the input file
        self.agmodule = self.GetModule()           # The name of the AgModule
        self.agpath   = self.options.path_name     # The path to store the exported files

        enable_warnings = not (self.input in legacy)
        
        try:
            os.mkdir( self.agpath )
        except OSError:            
            pass # Really should detect "already there" vs "can't create"
               
        head = self.agpath +'/'+ self.agmodule + '.h'
        impl = self.agpath +'/'+ self.agmodule + '.cxx'
        
        self.head = IOHandler(newline=True)
        self.head.connect(unit='out', file=open(head,'w'))   # would prefer to do | indent - -o head        

        self.impl = IOHandler()
        self.impl.connect(unit='out', file=open(impl,'w'))

        glob = IOBuffer()
        glob.setFile( self.impl )
        self.impl.connect(unit='global', file=glob )



    def endElement(self,tag):
        #
        # Flush buffers in implementation file
        #
        myglobal = self.impl.unit('global')
        mymodctr = self.impl.unit('modctr')
        if myglobal: myglobal.dump()
        if mymodctr: mymodctr.dump()
        #
        for name in self.blocks:
            myblock=self.impl.unit(name)
            if myblock: myblock.dump()

        #
        self.impl.write( '// ----------------------------------------------------------------------- geoctr\n' )

        mygeoctr = self.impl.unit('geoctr')
        if mygeoctr: mygeoctr.dump()

        # Final sanity check for options
        for volume in self.content:
            if volume in self.blocks:
                pass
            else:
                ##raise MissingError(volume,self.agmodule)
                RaiseException( MissingError( volume,self.agmodule ) )
        
        
    def GetModule(self):

        array  = self.input.split('/')
        infile = array[len(array)-1]
        module = infile.split('.')[0]
        if ( self.options.module_name != None ):
            module = self.options.module_name
        return module
#______________________________________________________________________________________________
# Begin support for new steering     
class Tag( Handler ):
    def __init__(self):
        global document
        self.parent   = None

        self.name     = None
        self.comment  = None
        self.flags    = []
        self.includes = []
        self.modules  = []

        # Build list of include files from either StarVMC/Geometry or $STAR/StarVMC/Geometry
        for root, dirs, files in os.walk( 'StarVMC/Geometry' ):
            for f in files:

                # Skip anything that looks like a backup file
                if '~' in f:
                    continue
                
                if 'Config.xml' in f:
                    name = f
                    name = name.replace('.xml','.h')
                    self.includes.append( 'StarVMC/StarGeometry/%s' % name )


                if 'Geo' in f and '.xml' in f:
                    name = f[:4].lower()
                    if '.' in name or '#' in name: continue
                    if name in self.modules:
                        pass
                    else:
                        self.modules.append(name)



        Handler.__init__(self)
        
    def setParent(self, p):
        self.parent = p

    def startElement(self, tag, attr ):
        self.name    = attr.get('name', None)
        self.comment = attr.get('comment', None)
        self.top     = attr.get('top', None )
        
        document.head( '#ifndef   __%s__tag__'  % self.name )
        document.head( '#define   __%s__tag__'  % self.name )

        for inc in self.includes:
            document.head( '#include "%s"'%inc )

        
        document.head( 'namespace StarGeometry { //$NMSPC'      )
        document.head( 'struct %s {'   % self.name )
        document.head( ' virtual ~%s(){ };' % self.name )
        document.head( ' static bool select();' )
        document.head( ' ClassDef(%s,1);'%self.name )
        document.head( '};' )
        document.head( '};' )
        document.head( '#endif  //__%s__tag__'  % self.name )

        


    def characters(self,content):
        for flag in content.split(' '):
            if len(flag.strip()):
                self.flags.append(flag)
                ## name = flag[:4]
                ## print '%s::setup();' % flag
                ## print '%s::Module *%s = new %s::Module();' % ( flag, name, flag )
                ## print '%s -> ConstructGeometry();' %name


    def endElement(self,tag):


        if self.parent.geometries == []:
            # Add module includes into header file
            for inc in self.includes:
                document.impl( '#include "%s"' % inc, unit='global' )
            
            for mod in self.modules:
                document.impl( 'AgModule *_%s = 0;'%mod, unit='global' )

            document.impl( '#include "StarVMC/StarGeometry/StarGeo.h"',            unit='global' )
            document.impl( '#include "StMessMgr.h"',                                    unit='global' )
            document.impl( '#include "TGeoManager.h"',                                  unit='global' )


        # Register this geometry with the document
        self.parent.geometries.append(self)

        document.impl( 'bool StarGeometry::%s::select() {\n'%self.name,                 unit='global' )                        
        document.impl( '// Create modules',                                         unit='global' )
        for flag in self.flags:
            name = flag[:4].lower()
            document.impl( '%s::setup();\n' % flag,                                 unit='global' )
            document.impl( '_%s = new %s::Module();\n'%(name,flag), unit='global' )
            document.impl( 'LOG_INFO << "Construct " << %s::name << "[%s]" << endm;'%{flag,flag})
        document.impl( '// Construct modules',                                      unit='global' )

# Geometry construction for later ...
#       for flag in self.flags:
#           name = flag[:4].lower()
#           document.impl( 'LOG_INFO << "[ " << %s::name() << " " << %s::module() << " " << %s::comment() << " ]" << endm;\n'%(flag,flag,flag), unit='global' )
#           document.impl( '%s -> ConstructGeometry();\n'%name,                     unit='global' )

        if self.top:
            document.impl( 'TGeoVolume *top = gGeoManager->FindVolumeFast("%s");\n'%self.top, unit='global' )
            document.impl( 'if (top) gGeoManager->SetTopVolume(top);\n',            unit='global' )
        document.impl( 'return true;\n',                                            unit='global' )
        document.impl( '};\n\n',                                                    unit='global' )


class StarGeometry(Handler):
    def __init__(self):
        self.parent = None
        self.name   = "StarGeometry"
        self.tag    = None
        self.geoms  = []
    def setParent(self, p):
        self.parent = p
    def addGeometry(self,geom):
        self.geoms.append(geom)
    def startElement(self,tag,attr):
        self.tag = attr.get('default','blackhole')
    def endElement(self,tag):
        global document
        
        header = """
#ifndef __StarGeometry_h__
#define __StarGeometry_h__
#include "TDataSet.h"
        #include <string>
        #include <map>
        class StarGeometry {
        public:
        /// Construct geometry with the specified tag, and return wrapped in a TDataSet
        static TDataSet* Construct( const char* name = "%s");
        static bool      List     ( const char* name = "%s");
        static void      Whitelist( const char* name, int value=1 ){ whitelist[name]=value; }
        StarGeometry(){ /* nada */ };
        virtual ~StarGeometry(){ /* nada */ }
        private:
        protected:
        static std::map<std::string,int> whitelist;                        
        ClassDef(StarGeometry,1);
        };

        // backwards compatability hack with StarDb/AgMLGeometry
        class Geometry : public StarGeometry {
        public:
        static TDataSet *ConstructGeometry( const char* name ){ return Construct(name); }
        Geometry():StarGeometry(){ /* nada */ };
        virtual ~Geometry(){ /* nada */ }
        ClassDef(Geometry,1);
        };
#endif        
        """%(self.tag,self.tag)
        document.head(header)

        implement1 = """
#include "StarVMC/StarGeometry/StarGeo.h"
#include "TObjectSet.h"
#include "TGeoManager.h"        
#include <string>
#include <map>

        std::map<std::string,int> StarGeometry::whitelist= {{"all",1}};
        
        TDataSet* StarGeometry::Construct( const char* name )
        {
        std::string tag = name;
        """
        document.impl( implement1, unit='global' )
        for geom in self.geoms:
            name    = geom.name;
            output = '             if (tag=="%s") { %s::construct( whitelist ); }'  %(name,name)          
            document.impl( output, unit='global' )
        document.impl( 'if (0 == gGeoManager) return NULL;', unit='global')
        document.impl( 'TObjectSet* dataset = new TObjectSet( "Geometry", gGeoManager, false );', unit='global' )
        document.impl( 'return (TDataSet*)dataset;', unit='global' )
        document.impl( '};',              unit='global' )        
        
        implement1 = """
        bool   StarGeometry::List( const char* name )
        {
        std::string tag = name;
        bool all = name==std::string("all");
        bool found = false;
        """
        document.impl( implement1, unit='global' )
        for geom in self.geoms:
            name    = geom.name;
            output = '             if (all||tag=="%s") { %s::list(); found = true; }'  %(name,name)         
            document.impl( output, unit='global' )
        document.impl( 'if ( 0==found ) LOG_INFO << tag << " not defined" << endm;', unit='global' )
        document.impl( 'return true;};',              unit='global' )        
        

        
        

class Geometry( Handler ):

    def __init__(self):
        self.parent   = None
        self.name     = None
        self.docum    = None
        self.modules  = []
        self.pmodules = []
        self.constructs = []
        self.sys    = [] # subsystems
        self.config = {} # subsystem configuration
        self.includes = ['StMessMgr.h'] # include files for dstector tags

        Handler.__init__(self)

    def addModule(self, module):
        self.modules.append(module)
        self.pmodules.append( module[:4].lower() )

    def addSystem(self, system, config ):
        self.sys.append(system)
        self.config[system] = config

    def setParent(self, p):
        self.parent = p

    def startElement(self, tag, attr ):
        self.name  = attr.get('tag', None)
        self.docum = attr.get('comment', None )       
        self.parent.addGeometry(self)

        # Build list of include files from either StarVMC/Geometry or $STAR/StarVMC/Geometry
        self.includes.append('StarVMC/StarGeometry/StarGeo.h')
        for root, dirs, files in os.walk( 'StarVMC/Geometry' ):
            for f in files:

                if f[:1] == '.':
                    continue # skip special files
                if '~' in f:
                    continue # skip backup files
                    
                if 'Config.xml' in f:
                    name = f
                    name = name.replace('.xml','.h')
                    self.includes.append( 'StarVMC/StarGeometry/%s' % name )

        ## self.name = attr.get('name', None)
        ## self.docum = attr.get('comment', None )

    def endElement(self,tag):
        global document
        document.head( '#ifndef __construct_%s_geometry__'%self.name )
        document.head( '#define __construct_%s_geometry__'%self.name )
        document.head( '#include <string>' )
        document.head( '#include <map>' )
#       document.head( 'namespace Star { //$NMSPC' )
        document.head( 'struct %s {' % self.name )
        document.head( 'static bool construct( std::map<std::string,int> whitelist = {{"all",1}} );' )
        document.head( 'static bool list();' )
        #document.head( '#if 0\n  ClassDef(%s,1);\n#endif\n'%self.name )
        document.head( '};' )
#       document.head( '};' )
        document.head( '#endif')


        for i in self.includes:
            document.impl('#include "%s"'%i, unit='global' )

        # Setup the builder code
        document.impl( 'bool %s::construct( std::map<std::string,int> whitelist ) {'%self.name, unit='global' )
        document.impl( 'bool result = true;',                    unit='global' )
        # Loop over detector tags.  Create in order and construct
        for sub in self.sys:
            subup = sub.upper();
            cfg = self.config[sub]
            if subup!="CAVE": # Always create the cave
                document.impl( 'if (whitelist["%s"]||whitelist["all"])'%(subup), unit='global' )
            document.impl( '{', unit='global' )
            document.impl( '\t%s::%s::setup();'%(subup,cfg), unit='global' )
            document.impl( '\t%s::%s::construct();'%(subup,cfg), unit='global' )
            document.impl( '} //endif [%s] '%(subup), unit='global' )
        document.impl( 'return result;\n',                      unit='global' )
        document.impl( '};',                                      unit='global' )

        # Setup code to list
        document.impl( 'bool %s::list() {'%self.name, unit='global' )
        document.impl( 'bool result = true;',                    unit='global' )
        # Loop over detector tags.  Create in order and construct
        for sub in self.sys:
            subup = sub.upper();
            cfg = self.config[sub]
            #ocument.impl( '%s::%s::setup();'%(subup,cfg), unit='global' )
            #ocument.impl( '%s::%s::construct();'%(subup,cfg), unit='global' )
            document.impl( 'LOG_INFO << "%s %s" << endm;'%(subup,cfg), unit='global' )
            document.impl( '%s::%s::list();'%(subup,cfg), unit='global' );
        document.impl( 'return result;\n',                      unit='global' )
        document.impl( '};',                                      unit='global' )
        

    
class Construct( Handler ):
    def __init__(self):
        self.parent = None
        self.sys    = None
        self.config = None
    def setParent(self, p):
        self.parent = p
    
    def startElement(self, tag, attr ):
        self.parent.constructs.append(self)
        self.sys = attr.get('sys',None)
        self.config = attr.get('config',None)
        
        self.parent.addSystem( self.sys, self.config )

        ## self.module = attr.get('module', None)
        ## self.track  = attr.get('track', 'primary' )
        ## self.parent.addModule( self.module )
        
class Detector( Handler ):
    """
    class Detector handles the detector tag in AgML

    <Detector name="NAME" comment="A documentation string">
    </Detector>

    The tag consists of two attributes: a name and a documentation string.  Its
    contents will specify various detector configurations, which will enable a
    concrete geometry construction code to realize the time evolution of the
    detector.
    """
            
    def __init__(self):
        global document
        self.parent = None

        self.name = None
        self.comment = None
        self.modules = []
        self.setups  = []

        Handler.__init__(self)

    def setParent(self, p):
        self.parent = p

    def addModule( self, module ):
        self.modules.append( module )

    def addSetup( self, setup ):
        self.setups.add( setup )

    def startElement(self, tag, attr ):
        global document
        self.name    = attr.get('name', None)
        self.comment = attr.get('comment', None)

        mymod   = document.GetModule()
        myguard = '__%s_%s__'% (mymod, self.name)
         
        #document.head( '#ifndef __%s_CONFIG__' % self.name )
        #document.head( '#define __%s_CONFIG__' % self.name )
        document.head( '#ifndef %s' % myguard )
        document.head( '#define %s' % myguard )        

        document.impl( '#include "StarVMC/StarGeometry/%s.h"\n\n' % document.agmodule )
        document.impl( '#include "StMessMgr.h"\n' )
        document.impl( '#include "TGeoManager.h"\n' )

    def endElement(self, tag ):
        # List include files for modules
        for module in self.modules:
            module = module.strip()
            if ''==module:
                continue
            document.head( '# include "%s/%s.h"'%( document.agpath, module ) )

        for setup in self.setups:
            document.head( setup )

        document.head( '#endif' )            

class Setup( Handler ):
    """
    class Setup specifies an individual detector configuration.
    """
    def __init__(self):
        self.parent = None
        self.name = None
        self.comment = None
        self.module = None
        self.onoff = None
        self.topvolume = None
        # flags (call agsflag ...)
        self.prin = None
        self.grap = None
        self.hist = None
        self.geom = None
        self.mfld = None
        self.debu = None
        self.simu = None
        self.flags = {}
        self.inits = []
        Handler.__init__(self);

    def setParent(self,p):
        self.parent = p

    def startElement(self, tag, attr ):
        self.name    = attr.get('name',    None)
        self.comment = attr.get('comment', None)
        self.module  = attr.get('module',  None)
        self.onoff   = attr.get('onoff',   None)
        self.topvolume = attr.get('top',   None)
        # Flags set by agsflag
        for flag in [ 'prin', 'grap', 'hist', 'geom', 'mfld', 'debu', 'simu' ]:
            myflag = attr.get(flag, None)
            if myflag:
                self.flags[flag] = int(myflag)


    def endElement(self, tag):

        # First, declare the object in the header file
        output = "namespace %s { //\n"% self.module.upper()[:4]
        output += 'struct %s {\n' % self.name 
        output +=  '  static const char *name()    { return "%s"; }\n'  % self.name 
        output +=  '  static const char *comment() { return "%s"; }\n'  % self.comment 
        output +=  '  static const char *module()  { return "%s"; }\n'  % self.module
        output +=  '  static       bool  list();\n'
        output +=  '  typedef %s::%s Module;\n'%( self.module.upper(), self.module ) 
        #output +=  '  static       bool  active; // true if detector configuration has been built\n' 
        #document.impl( '  bool %s::active = false;\n'%self.name );
        if "on" in self.onoff.lower():
            output +=  '  static const bool onoff = true;\n'
            output +=  '  static       AgModule* New(){ return new Module(); }\n'            
        else:
            output +=  '  static const bool onoff = false;\n'
            output +=  '  static       AgModule* New(){ return NULL; }\n'
            
        output +=  '  static       void  setup();\n'
        output +=  '  static       AgModule* construct();\n'
        
#       output +=  '#if 0\n'
#       output +=  'ClassDef(%s,1);\n'%self.name
#       output +=  '#endif\n'
        output +=  '};\n'
        output +=  '};\n'

        # 
        self.parent.setups.append(output)
        
        # Wrap in namespace
        nmspc = self.module.upper()[:4]

        #
        # Add SETUP
        #
        output = "\n"
        output += "void %s::%s::setup() {\n"%(nmspc,self.name)
        document.impl( output, unit='global' )

        for init in self.inits:
            init.implement()

        output = "\n};\n"
        document.impl( output, unit='global' )

        #
        # Add List
        #
        output = "\n"
        output += "bool %s::%s::list() {\n"%(nmspc,self.name)
        flaglist = "";
        for flag in [ 'prin', 'grap', 'hist', 'geom', 'mfld', 'debu', 'simu' ]:
            try:
                value = self.flags[flag]
                #                output += 'LOG_INFO << "  FLAG %s = %i" << endm;'%(flag,value)
                flaglist += '" %s = %i" << '%(flag,value)
            except KeyError:
                pass

        output += 'LOG_INFO << name() << " " << module() << %s " [" << comment() << "]" << endm;\n'%flaglist
        document.impl( output, unit='global' )
        for init in self.inits:
            init.list()
        output  = "return true;\n};\n"
        document.impl( output, unit='global' )


        #
        # Add CONSTRUCT
        #
        output = "\n"
        output += "AgModule* %s::%s::construct() {\n"%(nmspc,self.name)
        output += 'LOG_INFO << "Construct module " << %s::%s::module() << endm;\n'%(nmspc,self.name)
        document.impl( output, unit='global' )

        #output = "active=true;\n"
        output  = "AgModule* _module = New();\n"
        output += "if (_module) _module->ConstructGeometry();\n"

        if self.topvolume:
            output += 'TGeoVolume* _top = gGeoManager->FindVolumeFast("%s");\n'%self.topvolume
            output += 'if (_top) {\n'
            output += '    gGeoManager->SetTopVolume(_top);'
            output += '} else { \n'
            output += '    LOG_WARN << "Could not find top volume %s... kaboom imminent..." << endm;\n'%self.topvolume
            output += '};\n'
        
        output += "return _module;"
        document.impl( output, unit='global' )
        
        output = "\n};\n"
        document.impl( output, unit='global' )
       
class Init( Handler ):
    def __init__(self):
        self.parent = None

    def setParent( self, p ):
        self.parent = p
        self.parent.inits.append(self)

    def startElement(self, tag, attr ):
        self.struct   = attr.get('struct', None )
        if self.struct: self.struct = self.struct.lower()
        self.variable = attr.get('var', None )
        self.value    = attr.get('value', None )
        self.index    = attr.get('index', None )
        self.namespace = self.parent.module.upper()

    def endElement(self, tag ):
        pass

    def implement(self):
        
        info = self.struct.replace("_t","_info")

        sinfo = '%s::%s_info'%(self.namespace,     info)
        minfo = sinfo + '::%s'%self.variable
        tinfo = minfo + '::Type'
        
        output  = "\n";
        output += "{\n";
        template = "%s,%s,%s"%(sinfo,minfo,tinfo)
        output += "AgMLDetp<%s>* %s_detp = \n\tAgMLDetp<%s>::New();\n"%( template, self.struct, template )
        output += "\t{\n"        
        output += "\t   %s temp = %s;"%(tinfo,self.value)
        output += "\t   memcpy( &%s_detp->member_value, &temp, sizeof(temp) );\n"%self.struct
        output += '\t   LOG_INFO << "  [AgML2.0] Runtime configure detector parameter %s.%s = %s" << endm;\n'%(self.struct,self.variable,self.value)

        output += "\t}\n"
        output += "}\n"
                
        document.impl( output, unit='global' )

    def list(self):
        
        output  = "\n";
        output += '\t   LOG_INFO << "  [AgML2.0] Runtime configure detector parameter %s.%s = %s" << endm;\n'%(self.struct,self.variable,self.value)
                
        document.impl( output, unit='global' )




class Modules( Handler ):
    """
    class Modules

    content specifies the list of modules which define a given detector 
    """
    def __init__(self):

        self.parent  = None
        self.modules = []
        Handler.__init__(self);

    def setParent(self,p):
        self.parent = p

    def characters(self, content):
        for module in content.split(','):
            module = module.strip()
            self.modules.append( module )
        self.parent.modules = self.modules
# End support for new steering
#______________________________________________________________________________________________
class Module ( Handler ):
    """
    class Module handles the module tag in AgML

    <Module name="NAME" comment="A documentation string">
    ...
    </Module>

    The tag contains two attributes: the name of the module and a documentation string,
    which is required (but not yet used).  The module is realized in c++ as a child
    class of AgModule.  User-code contained within the module will be inserted into
    the AgModule::ConstructGeometry() method.
    
    """

    def __init__(self):
        self. name    = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        #self. structs = [] # data structures

        Handler.__init__(self)
    def setParent(self,p): self.parent = p

    def startElement(self, tag, attr):
        global current, current_block
        
        name = attr.get('name','NONE')
        comm = attr.get('comment','NONE')

        current_block = name

        self.name    = name
        self.comment = comm
        
        _in_module = True

        # ---------------------------------------------------------------
        # Setup the module output stream in the file handler
        # ---------------------------------------------------------------
        modctr = IOBuffer()
        modctr.setFile( document.impl )
        document.impl.connect( unit='modctr', file=modctr )
        current = 'modctr'
        
        geoctr = IOBuffer()
        geoctr.setFile( document.impl )
        document.impl.connect( unit='geoctr', file=geoctr )
        current = 'geoctr'


        # ---------------------------------------------------------------    
        # Global scope of implementation file
        # ---------------------------------------------------------------        
        document.impl('#include "%s.h"' % document.agmodule, unit='global')
        document.impl( banner, unit='global' )
##        document.impl('ClassImp(%s);'   % document.agmodule, unit='global')
        document.impl(seperator, unit='global')
        document.impl(skip, unit='global')
        document.impl('#include "StarVMC/StarAgmlLib/StarAgmlStacker.h"', unit='global')
        document.impl(skip, unit='global')
        for h in ["AgMaterial","AgMedium","AgShape","AgBlock","AgMath","AgSTAR"]:
            document.impl( text='#include "StarVMC/StarAgmlLib/%s.h"'%h, unit='global' )
        document.impl(skip, unit='global')
        for h in ["Mortran","AgMath"]:
            document.impl( text='#include "StarVMC/StarAgmlLib/%s.h"'%h, unit='global' )
        for h in ["iostream", "vector", "map"]:
            document.impl( text='#include <%s>'%h, unit='global' )

        document.impl('const Int_t _printlevel = 0;', unit='global' )
        document.impl('#define LOG_PRINT if(_printlevel>0) std::cout << GetName() << " -Print- "', unit='global')
        document.impl('#define LOG_INFO  if(_printlevel>1) std::cout << GetName() << " -Info-  "', unit='global')
        document.impl('#define LOG_DEBUG if(_printlevel>2) std::cout << GetName() << " -Debug- "', unit='global')
        document.impl('#define LOG_WARN  if(_printlevel>3) std::cout << GetName() << " -Warn-  "', unit='global')

        document.impl('#define printf(fmt,...) LOG_PRINT << Form(fmt,##__VA_ARGS__) << std::endl;', unit='global')
        document.impl('#include "StarVMC/Geometry/Helpers.h"', unit='global')

        document.impl(skip,unit='global')
        if namespace:
            document.impl( 'namespace %s // $NMSPC'%document.agmodule.upper(), unit='global' )
            document.impl( '{', unit='global' )

        document.impl(skip, unit='global')
        document.impl(seperator, unit='global' )

        # ---------------------------------------------------------------
        # Header file
        # ---------------------------------------------------------------        
        document.head('#ifndef __%s__' % document.agmodule )
        document.head('#define __%s__' % document.agmodule )
        document.head( banner );
        document.head('')
        document.head('#include "StarVMC/StarAgmlLib/AgModule.h"')
        document.head('#include "StarVMC/StarAgmlLib/AgMLStructure.h"')
        document.head('#include <stddef.h>')
        document.head('')


        # Insert namespace
        if namespace:
            document.head('namespace %s // $NMSPC'%document.agmodule.upper())
            document.head('{')


        # ---------------------------------------------------------------
        # Add class constructor
        # ---------------------------------------------------------------        
        document.impl('%s::%s()' % (document.agmodule, document.agmodule),     unit='modctr' )
        document.impl('  : AgModule("%s","%s")' % ( document.agmodule, comm ), unit='modctr' )
        document.impl('{',                                                     unit='modctr' )
        document.impl('}',                                                     unit='modctr' )

        document.impl('void %s::ConstructGeometry( const Char_t *dummy )' % document.agmodule,      unit=current  )
        document.impl('{',                                                     unit=current  )

    def characters( self, content ):
        content = content.lstrip()
        content = content.rstrip()
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
            content += ';'
        document.impl(content, unit=current)

    def endElement(self, tag ):
        _in_module = False

        # ---------------------------------------------------------------
        # Header file
        # ---------------------------------------------------------------
        document.head('/// \class %s'%document.agmodule )
        document.head('/// \\brief %s'%self.comment      )
        document.head('class %s : public AgModule' % document.agmodule )
        document.head('{')
        document.head('public:')
        document.head('%s();' % document.agmodule )
        document.head('virtual void ConstructGeometry( const Char_t *dummy="" );')
        document.head('~%s(){ };' % document.agmodule )

            
        document.head('public:')        
        document.head('};')

        document.head('// endElement in class Module')
        if namespace:
            document.head('}; // namespace %s'%document.agmodule)
        

        document.head('#endif // __%s__' % document.agmodule)
        # End geomctry constructor
        document.impl('}; // %s'%document.agmodule, unit= 'geoctr' )
        # and close namespace
        if namespace:
            document.impl('}; // namespace %s'%document.agmodule, unit='geoctr')
            
    def addVar(self,name,type,dim,value,comment):
        if ( comment != None ):
            pass
        else:
            pass
class Block( Handler ):

    def __init__(self):
        self. name     = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Handler.__init__(self)    

    def setParent(self,p): self.parent = p
    def startElement(self, tag, attr):
        global current, current_block
        
        name     = attr.get('name','NONE')
        comm     = attr.get('comment',None)
        assembly = attr.get('assembly',False)
        if assembly:
            comm += " [transformed into assembly in TGeo]"
        else:
            comm += " [TGeoVolume]"

        # Error checking
        if len(name)!= 4:
            ##raise AgmlNameError( name, tag )
            RaiseException( AgmlNameError(name,tag) )
        if comm==None:
            ##raise AgmlCommentError( name, tag )
            RaiseException( AgmlCommentError(name,tag) )
        
        self.name      = name
        self.comment   = comm
        self.assembly  = assembly

        current_block = name
        
        _in_block = True

        # ---------------------------------------------------------------
        # Add a ClassImp to the global section
        # ---------------------------------------------------------------
##        document.impl('ClassImp(%s);' % name, unit='global')

        # ---------------------------------------------------------------
        # Setup the block output stream in the file handler
        # ---------------------------------------------------------------
        blkctr = IOBuffer()
        blkctr.setFile( document.impl )
        document.impl.connect( unit=name, file=blkctr )
        current = name

        if name in document.content:
            document.blocks.append( name )
        else:            
            ##raise ContentError( name, document.agmodule )
            RaiseException( ContentError(name,document.agmodule) )
        
        

        # ---------------------------------------------------------------
        # Add the block definition to the header file, appending documentation
        # ---------------------------------------------------------------
        document.head('// ---------------------------------------------------------------------- %s --' % name )
        document.head('///@defgroup %s_doc'%self.name)
        document.head('///@class %s'%self.name)
        document.head('///@brief %s'%str(self.comment) )
        document.head('class %s : public AgBlock' % name )
        document.head('{  public:')
        document.head('   static AgBlock *Instance();' )
        document.head('%s() : AgBlock("%s","%s"){'%(name,name,comm))
        if assembly:
            document.head('  mMakeAssembly=true;')
        document.head('};')
        document.head('~%s(){ };'%name)
        document.head('virtual void Block( AgCreate c );')
        document.head('virtual void End(){ };')
        document.head('protected:')
        document.head('  static AgBlock *mInstance;')
        #document.head('ClassDef(%s,1);'%name)
        document.head('};')

        # ---------------------------------------------------------------
        # Provide some whitespace at the start of the block
        # ---------------------------------------------------------------
        document.impl('\n', unit=current )
        document.impl(seperator, unit=current )

        # ---------------------------------------------------------------
        # Add the block builder to the block stream
        # ---------------------------------------------------------------
        document.impl('AgBlock *%s::mInstance = 0;'%name, unit=current)
        document.impl('AgBlock *%s::Instance(){'%name,unit=current)
        document.impl('   if ( mInstance==0 ) mInstance = new %s();'%name,unit=current)
        document.impl('   return mInstance;',unit=current)
        document.impl('}',unit=current)
        document.impl('void %s::Block( AgCreate create )' % name, unit=current)
        document.impl('{ ', unit=current )
        document.impl('///@addtogroup %s_doc'%name, unit=current);
        document.impl('///@{', unit=current )
        document.impl('  AgBlock *_save = mCurrent;', unit=current);
        document.impl('  mCurrent = this;', unit=current);
        
        # ----------------------------------------------------------------
        # Detection of a new volume
        # ----------------------------------------------------------------
        document.impl('  Bool_t _same_shape = true;', unit=current)

    def endElement(self, tag ):
        _in_block = False
        # ---------------------------------------------------------------
        # Close the current block
        # ---------------------------------------------------------------
        document.impl('END_OF_%s:'%self.name, unit=current)
        document.impl('mCurrent = _save;',unit=current)
        document.impl('///@}', unit=current)
        document.impl('} // End Block %s' % self.name, unit=current )

    def characters( self, content ):
        """
        Insert character content into the current block or module
        """
        content = content.lstrip()
        content = content.rstrip()
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
            content += ';'             
        document.impl( content, unit=current )
class Group( Handler ):
    """
    
    """
    def __init__(self):
        self. name     = ""
        self. comment = "DOCUMENTATION NOT PROVIDED"
        Handler.__init__(self)    

    def setParent(self,p): self.parent = p
    def startElement(self, tag, attr):
        global current, current_block
        
        self.name = attr.get('name',   None)
        self.comm = attr.get('comment',None)
        self.cond = attr.get('if',     None)
        if self.cond:
            self.cond = replacements(self.cond).lower()

        if self.cond:
            document.impl('if (%s)'%self.cond, unit=current)
        document.impl('{ AddGroup("%s"); }'%(self.name), unit=current)
class Export( Handler ):
    """
    Language-specific language block.  Code wrapped in an export block will only
    be output to the export file if the language attribute contains AgROOT
    """
    def __init__(self):
        self.name = "AgROOT"
    def startElement(self, tag, attr):
        """
        Set IOHandler export flag based on language specified in the export block
        """
        language = attr.get('language',None)
        if language:
            _isExporting = language.count(self.name)
            if _isExporting:
                setExport(True)
            else:
                setExport(False)
                
    def endElement(self,tag):
        """
        Restore export when exiting an export block
        """
        setExport(True)
class Subroutine ( Handler ):
    """
    Handles the definition of subroutines and their mapping from the F/Mortran subroutines
    into c/c++ functions with arguements passed by reference.
    """
    def __init__(self):
        self.code = [] # Lines of code to be executed
        self.rvalue = '' # Return value
        self.rtype  = ''
        self.arglist = []
        Handler.__init__(self)
        
    def setParent(self,p): self.parent = p
    def addArguement(self,name,type,dim,value,comment):
        pass
    
    def startElement(self,tag,attr):
        global document
        global current

        self.name    = attr.get('name').lower()
        self.rtype   = attr.get('type','void')
        self.comment = attr.get('comment','This method has not been documented')
        self.args    = attr.get('args',[])

        # ---------------------------------------------------------------
        # Setup the block output stream in the file handler
        # ---------------------------------------------------------------
        blkctr = IOBuffer()
        blkctr.setFile( document.impl )
        document.impl.connect( unit=self.name, file=blkctr )
        current = self.name
        document.blocks.append( self.name )               
            
    def characters(self, content):
        content = content.lstrip()
        content = content.rstrip()
        if not len(content): return
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
            content += ';'
            
        document.impl(content, unit=current)

            
    def endElement(self,tag):
        
        # Build arg list for this subroutine        
        mydef = '('
        for i,arg in enumerate(self.arglist):
            if arg['intent']=='inout' or arg['intent']=='out':
                mydef += '%s &%s'%( arg['type'], arg['name'] )
            else:
                mydef += '%s  %s'%( arg['type'], arg['name'] )
            if arg['default']:
                mydef += '=%s'%arg['default']
            if i<len(self.arglist)-1:
                mydef += ','
        mydef += ')'
             
        document.impl.prepend( '%s %s%s {'%(self.rtype, self.name, mydef), unit=current )
        document.impl( '} // subroutine %s' % self.name, unit=current )
        
        #document.impl.unit(self.name).dump()
        
        comment = self.comment.strip()

        if namespace:
            #document.impl( 'namespace %s // $NMSPC'%document.agmodule.upper(), unit='global' )
            #document.impl( '{', unit='global' )
            document.head( 'namespace %s // $NMSPC'%document.agmodule.upper() )
            document.head( '{' );
        document.head('/// %s'%comment )
        document.head('extern %s %s %s;'%(self.rtype,self.name,mydef) )

        if namespace:
            document.head( '}' );
class Assign( Handler ):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):

        var = attr.get('var', None)
        val = attr.get('value', None)

        if val==None: val='0'
        if val.strip()[0] == '{':  # array assignment

            array = val.rstrip(';').strip('{}').replace(';',',').split(',')
            for i,a in enumerate(array):
                a = replacements(a.lower())
                document.impl('%s.at(%i) = %s;' % (var.lower(), i, a.strip()), unit=current )

        else:
            val=replacements(val.lower())
            document.impl( '%s = %s;'%( var.lower(), val ), unit=current )
class Keep( Handler ):
    def __init__(self):
        self.export = 'All'
        Handler.__init__(self)    
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        pass
        
    def endElement(self,tag):
        pass

    def characters(self,contents):
        if len(contents.strip())>0:
            document.impl( contents.strip(), unit=current )
class Content ( Handler ):

    def setParent(self,p): self.parent = p    
    def __init__(self):
        self. content = False
        Handler.__init__(self)
    
    def characters( self, content ):
        blocks = content.strip().split(',')
        for block in blocks:
            block=block.strip()
            if len(block):
                document.impl( 'AddBlock("%s", %s::Instance());' %(block,block), unit=current ) # ought to be modctr
                document.content.append('%s'%block)
class Include ( Handler ):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement( self, tag, attr ):
        file = attr.get('file',None)
        if ( file != None ):
            #form( "#include \"%s\"" % file )
            pass
class Cde ( Handler ):
    cde = False
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        pass
    def characters(self, char):
        if ( len( char.strip() ) > 0 ):
            #form( "+CDE,"+char.lower().strip()+"." )
            pass

_inlined_functions = {}
class Inline( Handler ):
    """
    So called 'inline' functions (more properly operator functions in mortran)
    are added to the list of functions contained in the Document class.  When
    the document's end tag is encountered, they are output to the implemntation
    nad header files.
    """
# TODO: In mortran export, Inline should add a new variable declaration
    def __init__(self):
        Handler.__init__(self)
        self.arglist = []
    def setParent(self,p):     	self.parent = p
    
    def startElement(self,tag,attr):
        global document, _inlined_functions
        
        typeMap = { 'real'             : 'Float_t',
                    'int'              : 'Int_t',
                    'real*4'           : 'Float_t',
                    'real*8'           : 'Double_t',
                    'double precision' : 'Double_t' }
        name    = attr.get('name')
        name    = name.lower()
        type    = attr.get('type','real')
        self.name = name
        
        try:
            lookup = _inlined_functions[ name ]
            return  # function already exported, should not repeat
        except KeyError:
            _inlined_functions[ name ] = False
        

        type=typeMap[type]
        self.type = type
        self.rvalue = ''
        
        # ---------------------------------------------------------------
        # Setup the block output stream in the file handler
        # ---------------------------------------------------------------
        blkctr = IOBuffer()
        blkctr.setFile( document.impl )
        document.impl.connect( unit=self.name, file=blkctr )
        #current = self.name
        document.blocks.append( self.name )             
        document.functions.append(self.name)

    def endElement(self,tag):
        global document, _inlined_functions

        # Skip if we have exported this function already
        if  _inlined_functions[ self.name ]:
            return
        else:
            _inlined_functions[ self.name ] = True
        
        mydef  = self.type + ' ' + self.name
        mydef += '('
        for i,arg in enumerate(self.arglist):
            mydef += '%s %s'%( arg['type'], arg['name'] )
            if arg['default']:
                mydef += '=%s'%arg['default']
            if i<len(self.arglist)-1:
                mydef += ','
        mydef += ') { return %s; }' % self.rvalue
        #print mydef

        # Add this inline function to the implementation file
        document.impl( mydef, unit=self.name )
class Arguement(Handler):
    """
    The Arguement class handles the <Arguement .../> keyword found in subroutine,
    function and (implicitly in) inline function definitions.
    """
    def __init__(self):
        self.parent = None
        Handler.__init__(self)
    def setParent(self,p): self.parent = p

    def startElement(self, tag, attr):
        typeMap = { 'real'             : 'Float_t',
                    'integer'          : 'Int_t',
                    'int'              : 'Int_t',
                    'real*4'           : 'Float_t',
                    'real*8'           : 'Double_t',
                    'double precision' : 'Double_t' }        

        name = attr.get('name',None)
        type = attr.get('type','real')
        val  = attr.get('default',None)
        intent = attr.get('intent','inout')
        if name:
            name=name.lower()

        type = typeMap[type]


        arg = { 'name': name, 'type' : type, 'default' : val, 'intent' : intent }

        self.parent.arglist.append( arg )
        
    def content(self, content):
        pass
    def endElement(self,tag):
        pass
class Return(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)                    
    def startElement(self,tag,attr):
        value = attr.get('value',None)
        if value:
            value = value.lower()
            value = replacements(value)
            self.parent.rvalue = value
class External( Handler ):
    def __init__(self):
        Handler.__init__(self)
    def startElement(self,tag,attr):
        #routine = attr.get('routine')
        #form( 'EXTERNAL %s' % routine )
        pass
    def endElement(self,tag):
        pass
class Import( Handler ):
    def __init__(self): Handler.__init__(self)
    def startElement(self,tag,attr):
        pass
    def endElement(self,tag):
        pass  
class Comment( Handler ):
    """
    All user comments will be stripped before export to AgROOT, because we will
    not know where to insert the comments into the different output streams which
    we are dealing with.
    """ 
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p
class Varlist( Handler ):

    def setParent(self,p):
        self.parent = p

    def __init__(self):

        self.mylist    = []
        self.arraylist = []
        self.arraydim  = []
        self.type   = None
        self.dim    = None
        self.len    = None
        Handler.__init__(self)        

    def startElement(self,tag,attr):
        
        type = attr.get('type',None)

        if ( type == None ):
            type = "Float_t"
        if ( type.lower() == 'real' ):
            type = "Float_t"
        if ( type.lower() == 'double precision' or type.lower() == 'real*8' ):
            type = "Double_t"

        # Detection of character*N declartions
        if type.find('*')>0:
            temp=type.split('*')
            temp[0]=temp[0].lower().strip()
            temp[1]=temp[1].lower().strip()
            if temp[0]=='character':
                type='TString'
                #size=temp[1]


        dim = attr.get('dim', None); self.dim = dim
        len = attr.get('len', None); self.len = len

        if ( type == "char" or type == "Char_t" ):
            type = "TString"
            
        if ( type.lower() == 'integer' ):
            type = "Int_t"

        self.type = type
        
    def characters(self,content):
        global _symbol_table

        index  = 0
        mylist = []
        arlist = []
        term   = ','
        var    = ''

        content = content.rstrip(',')
        content = content.strip()
        if content=='': return

        while index<len(content):
            c = content[index]

            if c==' ':
                index+=1
                continue  # skip whitespace
            if c=='(':
                term=')'  # switch terminator on paren

            index+=1
            if index==len(content): # terminate at end of list
                term=c

            if c==term:          # termination of a variable

                if not c==',':
                    var += c         # termination may be part of variable
                
                if c==')':       # on arrays, append and reset terminator
                    arlist.append( var )
                    term = ','
                    var=''
                    continue
                else     :       # on scalars, append
                    var.rstrip(',') # strip off the , if it was appended
                    if len(var):
                        mylist.append( var )
                    var=''
                    continue

            var += c

        for var in mylist:
            if var.find('/')>0:
                temp=var.split('/')
                var='%s=%s'%( temp[0], temp[1] )
            self.mylist.append(var)

        for var in arlist:
            self.arraylist.append(var)
            
                
    def endElement(self,tag):
        global _symbol_table
        # TODO: symbol_table should be moved into the Module and Subroutine classes        
        
        for var in self.mylist:
            _symbol_table[var] = self.type # Set the type of the current variable
        for arr in self.arraylist:
            _symbol_table[arr] = 'Array_t<%s>'%self.type
            
        # Lower-case all variables in mylist
        for i,f in enumerate(self.mylist):
            self.mylist[i]=f.lower()
            
        # Strip inline functions from lists
        for f in document.functions:
            if f in self.mylist:
                del self.mylist[ self.mylist.index(f) ]
            if f in self.arraylist:
                del self.arraylist[ self.arraylist.index(f) ]
                
        # Strip AgML functions
        for f in agml_functions:
            if f in self.mylist:
                del self.mylist[ self.mylist.index(f) ]

        varlist = self.type + ' ' + ( ','.join(self.mylist) ).lstrip().lower()
                
        myunit = 'global'
        if self.parent.__class__ == Subroutine:
            myunit = current

        document.impl('///@addtogroup %s_vars'%document.agmodule, unit=myunit )
        document.impl('///@{', unit=myunit )
                        
        if len(self.mylist):
            document.impl( varlist + ';',  unit=myunit )
            document.impl( skip,           unit=myunit )
            document.impl( '/// %s'%varlist, unit=myunit )

        for arr in self.arraylist:

            if arr.find(':')>0:
                temp1 = arr.split(':')
                myname = temp1[0].split('(')[0]
                document.impl( 'Array_t<%s> %s(1);'%( self.type, myname.lower() ), unit=myunit )
                temp2  = arr.rstrip(')').split('(')
                temp3  = temp2[1].split(':')                
                document.impl( '%s.SetRange(%s,%s);'%( myname.lower(), temp3[0].lower(), temp3[1].lower() ), unit=current ) # Range is set in code, not in global
                document.impl( '/// %s : array of %s : low=%s upper=%s'%(myname.lower(),self.type,temp3[0],temp3[1]), unit=myunit)
                continue
            
            if len(arr)>0:
                document.impl( 'Array_t<%s> %s;'     %( self.type, arr.lower() ), unit=myunit )
                document.impl( '/// %s : array of %s'%(arr.lower(),self.type), unit=myunit )              
                continue

        document.impl('///@}', unit=myunit )            
class Var ( Handler ):

    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.type    = None
        self.content = False
        Handler.__init__(self)                

    def startElement(self,tag,attr):

        name = attr.get('name')
        type = attr.get('type','float')
       
        if ( type == 'float' ):
            type = 'real'
        if ( type == 'double' ):
            type = 'double precision'

        value   = attr.get('value',None)
        comment = attr.get('comment',None)
        dim     = attr.get('dim',None)

        value = replacements(value) # could (illegally) reference

        if dim==None:
            mylist = name.strip(')').split('(')
            if len(mylist)>1:
                dim  = mylist[1]
                name = mylist[0]

                #if int(dim)>2:
                #   raise AgmlArrayError( name.lower(), reason='AgML supports only 1 and 2D arrays' )

        if ( name != None ):
            try:
                self.parent.addVar(name,type,dim,value,comment)
            except AttributeError:
                print 'AttributeError: %s does not implement addVar' % self.parent
                print '              : name    %s'%name
                print '              : type    %s'%type
                print '              : dim     %s'%dim
                print '              : value   %s'%value
                print '              : comment %s'%comment
                assert 2+2==5
            
        self.type = type
        
    def characters(self,content):
        # We do not allow character data to be present in a Fill statement
        content = content.lower()
        content = replacements(content)        
        pass 
class Data( Handler ):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        pass
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

        #
        # Get the name and the type of the variable.  If the type was not
        # set, we may be running ageParser directly (i.e. the parameter 
        # declaration appears after the variable declaration ala fortran).
        # In that case we consult the symbol table.
        #        
        name = attr.get('name')
        type = attr.get('type', None )
        
        if type==None:
            type = _symbol_table[name]

        
        # We remap fortran to c variables
        table = { 'integer' : 'Int_t',
                  'real'    : 'Float_t',
                  'double precision' : 'Double_t',
                  'real*4' : 'Float_t',
                  'real*8' : 'Double_t',
                  'int_t'  : 'Int_t',
                  'float_t' : 'Float_t',
                  'double_t' : 'Double_t' }

        mytype = type.replace(' ','').lower()
        type = table[mytype]        
        name = attr.get('name')
        value = attr.get('value')
        
        # Add the parameter definition to the module constructor
        #document.impl( '%s=%s;'%( name.lower(), value.lower()), unit='modctr')
        document.impl( '%s=%s;'%( name.lower(), value.lower()), unit=current)
class Enum( Handler ):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
class Struct( Handler ): # new style structures

    def setParent(self,p): self.parent = p    
    def __init__(self):

        self. name = 0
        self. var_list = [] # variable names
        self. var_dict = {} # type 
        self. var_dims = {} # dimensions
        Handler.__init__(self)

    def addVar(self,name,mytype,dim,value,comment):
        global _struct_table
        global _struct_dims
        
        table = { 'real'    : 'float',
                  'double'  : 'double',
                  'int'     : 'int',
                  'integer' : 'int',
                  'float'   : 'float',
                  'Float_t' : 'float',
                  'Double_t': 'double',
                  'Int_t'   : 'int',
                  'char'    : 'string' }        

        # Nothing to do if passed a null string (WHY SILENT HERE?)
        if len(name.strip()) == 0:            return

        # Get the mytype of the variable
        mytype = table[mytype]

        # All variable names are lower case
        name = name.lower()

        self.var_list.append(name)
        self.var_dict[name] = mytype
        self.var_dims[name] = dim

    def startElement( self, tag, attr ):

        # Name of the structure
        name      = attr.get('name',None)
        self.name = name.lower()

        # Append name of strcture to all structs in document
        document.structs.append( name.lower() )

        # Add to global 
        _structures[ name+ _agml_sep ] = name+_struct_sep
       
    def endElement( self, tag ):
        global _struct_table
        global _struct_dims

        # Add an index to the structure to enable selection of default
        self.addVar(name='_index',mytype='int',dim=None,value=None,comment='!$ agml index')
        
        # Name of the c-structure
        name = self.name.lower()

        # Make type dictionary persistent
        _struct_table[ self.name ] = self.var_dict

        # Name of the structure wrapper which handles the fill/use interface 
        wrap = name.upper()

        # First stage: declare plain c-structure
        struct = "struct %s_t {\n" % name
        for var in self.var_list:
            typ = self.var_dict[var] # type of the variable
            dim = self.var_dims[var] # dimension of the variable
            if dim == None:               dim = ""
            else:
                key = '%s.%s'%(self.name.lower(), var.lower())
                _struct_dims[key] = dim



                # Transform array a(1,2,3) to a[3][2][1]
                dim = dim.strip('()')
                length = dim.split(',')
                dim = ""
                for i in reversed(length):
                    dim += "[%s]"%i
                                
#                dim = "[%s]"%dim
#                # hack to handle 2d arrauys
#                dim = dim.replace(',', '][')

            struct += "%s %s%s;\n"%( typ, var, dim )
        struct += "};\n"


        # Second, setup documentation structure
        docum =  "struct _%s_docum_ {\n"%name
        for var in self.var_list:
            docum += "string %s;\n"%var
        docum += "};\n";

        # Third, setup a typedef lookup
        typedef = " struct _%s_types_ {\n"%name
        for myvar in self.var_list:
            mytype = self.var_dict[myvar]
            mydim  = self.var_dims[myvar] # dimension of the variable
            if mydim != None:

                mydim = mydim.strip('()')
                length = mydim.split(',')
                mydim = ""
                for i in reversed(length):
                    mydim += '[%s]'%i                                
                #mydim = '[' + mydim + ']'
                #mydim = mydim.replace(',','][')
            else:
                mydim=''
            ##     mytype += mydim
            
            typedef += "  typedef %s %s%s;\n"%(mytype, myvar, mydim)
        typedef += "};\n"

        # Fourth, setup a member info ... dictionary

        membid  = "\n#ifndef __CINT__\n";
        membid += "\nstruct %s_info {\n"%name

        membid += 'typedef %s_t Type;\n'%name
#       membid += 'Type prototype;\n';                                       # NEW 05/11/16
        membid += 'static const char *name(){ return "%s_t"; }\n'%name

        lastvar = None
        for myvar in self.var_list:
            mytype = self.var_dict[myvar]
            mydim  = self.var_dims[myvar] # dimension of the variable
            membid += "struct %s {\n"%myvar
            membid += '  static const char *name() { return  "%s"; }\n'%myvar

            membid += '  static size_t Offset(){ return offsetof( struct %s_t, %s ); }\n'%(name,myvar)
            
#           if lastvar == None:
#               membid += '  static int Offset(){ return 0; }\n'
#           else:
#               membid += '  static int Offset(){ return %s::Offset()+sizeof(%s::Type); }\n'%(lastvar,lastvar)


            membid += '  static void* address(){ \n'
            membid += '         size_t iadd = (size_t)%s_info::address;\n'%name
            membid += '         iadd += Offset();\n'
            membid += '         return (void *)iadd;}\n'

            #
            # FIX THIS PLEASE.  We output the dimension of the array, but we need
            # to know how many indicies it has.  i.e. is this array hx[10] or hx[2][5]?
            #

            # Base type is the type of the variable w/out array indicies
            membid += "  typedef %s BaseType;\n" %mytype
            
            if mydim == None:
                membid += "  typedef %s Type;\n" %mytype
                membid += "  static const int rank = 0;"
            else:
                #mydim = mydim.replace(',','][')
                mydim = mydim.strip('()')
                length = mydim.split(',')
                mydim = ""
                for i in reversed(length):
                    mydim += '[%s]'%i
                                    
                membid += "  typedef %s Type%s;\n" % (mytype,mydim)
                #membid += "  static const int rank = %s;"%mydim

            membid += "};\n"
            lastvar = myvar;

        membid += "static void              *address;\n"
        membid += "static AgMLStructureBase *structure;\n"
            
        membid += "};\n"
        membid += "#endif\n";


        dictionary  = "\n\tvoid              *%s_info::address   = &%s;\n"%(name,name)
        dictionary += "\tAgMLStructureBase   *%s_info::structure = &%s;\n"%(name,name.upper())

        # Setup search predicates for each scalar variable
        preds = "// Selectors for %s\n"%name
        for var in self.var_list:
            typ = self.var_dict[var] # type of the variable
            dim = self.var_dims[var] # dimension of the variable
            if dim == None:
                preds += "isEqual<%s_t, %s> __%s_%s__( __%s, &__%s->%s );\n"%(
                    name,
                    typ,
                    name,
                    var,
                    name,
                    name,
                    var
                    )

        # preds += "delete __%s; __%s = 0;\n\n"%(name,name) # cleanup memory but needs to be in body of code

        # Setup table wrapper, and get reference to the structure 
        wrapper  = "%s_t *__%s = new %s_t; // dummy\n "%( name, name, name )       
        wrapper += 'AgMLStructure<%s_t,%s_info> %s("%s");\n'%(name,name,name.upper(),name)
        wrapper += "%s_t &%s = %s;\n"%(name,name,name.upper())
        

        # Place definitions in the header file
        document.head( struct )
        document.head( docum )
        document.head( typedef )
        document.head( membid )

        # Place declarations into implementation file
        document.impl( wrapper, unit='global' )
        document.impl( preds,   unit='global' )
        document.impl( dictionary, unit='global' )
class Structure( Handler ):

    def setParent(self,p): self.parent = p    
    def __init__(self):

        self. name = 0
        self. var_list = []
        self. var_dict = {}
        self. ctor = ""
        self. newtor = ""
        Handler.__init__(self)

    def addVar(self,name,type,dim,value,comment):
        global _struct_table
        table = { 'real'    : 'Float_t',
                  'double'  : 'Double_t',
                  'int'     : 'Int_t',
                  'integer' : 'Int_t',
                  'float'   : 'Float_t',
                  'Float_t' : 'Float_t',
                  'Double_t': 'Double_t',
                  'Int_t'   : 'Int_t',
                  'char'    : 'TString' }

        # Nothing to do if passed a null string (WHY SILENT HERE?)
        if len(name.strip()) == 0:
            return
        
        if ( dim != None ):
            type = 'Array_t<%s>' % table[type]
            self.ctor += '%s(%s),'% (name.lower(), dim)
            self.newtor +='%s = %s(%s);'%(name.lower(),type,dim)

            #if int(dim)>2:
            #    raise AgmlArrayError( name.lower(), self.name, 'AgML supports only 1 and 2D arrays' )
            
        else:
            type = table[type]
            self.ctor += '%s(0),' % name.lower()
            if type=='TString':
                self.newtor += '%s="";' % (name.lower())
            else:
                self.newtor += '%s=0;' % (name.lower())

        self.var_list.append( name.lower() )
        self.var_dict[ name.lower() ] = type
               
    def startElement( self, tag, attr ):
        
        name      = attr.get('name',None)
        self.name = name.lower()
        document.structs.append( name.lower() )       
        _structures[ name+ _agml_sep ] = name+_struct_sep

                
    def endElement( self, tag ):
        global _struct_table
        
        name=self.name

        self.addVar(name='_index',type='Int_t',dim=None,value=None,comment='!$ agml index')
        self.mkdocs = True

        if self.mkdocs:
            document.impl('//  -----------------------------------------------------',       unit='global')
            document.impl('/// @defgroup %s_doc'%name,           unit='global') # doxygen group is lowercase struct name
            document.impl('/// \class %s_t'%camelCase(name),     unit='global') # declares it as a class (which it is)
            document.impl('/// \\brief User-defined structure',  unit='global') # provides a brief description
            document.impl('///                        ',         unit='global') #        
            document.impl('/// AgML structure members:',         unit='global') # followed by list of structure members
            document.impl('///                        ',         unit='global') #
        
        #document.head('struct %s_t' % camelCase(name))
        #document.head('{',                           )

        self.ctor  = self.ctor.rstrip(', ')
        self.ctor += '{ }'

        for v in self.var_list:

            type = self.var_dict[ v ]

            if ( type == 'real' ):
                type = 'Float_t'
                
            if ( type == 'int'  ):
                type = 'Int_t'
                
            if ( type == 'char' ):
                type = 'TString'
                
            document.impl(text='///%s %s;'%( type, v ), unit='global')

        document.impl(skip,                                        unit='global')
        
        # Make type dictionary persistent
        _struct_table[ self.name ] = self.var_dict

        ################################################################################
        #
        # New version refactoring the code to AgStructure
        #
        document.head('class %s_t : public AgStructure' % camelCase(name) )
        document.head('{')
##        document.head('ClassDef(%s::%s_t,1);'%(document.agmodule,camelCase(name)))
        document.head('ClassDef(%s_t,1);'%(camelCase(name)))        
        
        document.head('public:')
        #
        # Redefine variable types
        for v in self.var_list:
            if len(v.strip())==0:                continue

            type = self.var_dict[ v ]
            if ( type == 'real' ):
                type = 'Float_t'
                
            if ( type == 'int'  ):
                type = 'Int_t'
                
            if ( type == 'char' ):
                type = 'TString'

            # Output variable type
            comment = "User-defined variable of type "+type
            if v == "_index":
                pass # _index provided in AgStructure
            else:
                document.head('%s %s;'%(type,v))

        document.head('%s_t() : AgStructure("%s_t","User-defined AgML structure")'%(camelCase(name),camelCase(name)))
        document.head('{')

        for line in self.newtor.split(';'):
            document.head('%s;'%line)
            
        document.head( '}')
        document.head( '~ %s_t(){ /* nada */ };'%camelCase(name))
        document.head('};')
        
##        document.impl('ClassImp(%s_t);'%camelCase(name),                          unit='global')
        document.impl('%s_t %s;'%(camelCase(name),name),                          unit='global')
        document.impl(skip,                                                       unit='global')
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

class Filling( Handler ):

    def setParent(self,p): self.parent = p    
    def __init__(self):

        self. name = 0
        self. commemt = 0

        self. var_list = [] # list of variables
        self. typ_list = [] # list of types
        self. val_list = [] # list of values
        self. com_list = [] # list of comments    
        
        Handler.__init__(self)

    def addVar(self,name,mytype,dim,value,comment):
        """
        The <var> tag will result in addVar being called.  This will append
        the variable name, type, value(s) and comments to the lists stored
        in this class.
        """    
        self.var_list.append(name.lower())
        self.typ_list.append(mytype)
        self.val_list.append(value)
        self.com_list.append(comment)

    def startElement(self,tag,attr):
        """
        On the start of a <Fill> tag we begin collecting the information
        about the elements of the struct which need to be filled, and'
        what values.
        """
        global document
        
        name = attr.get('name',   None);     self.name = name.lower()
        comm = attr.get('comment',None);     self.comment = comm        
                   

    def endElement(self,tag):
        global _struct_table, document

        document.impl( seperator, unit=current)
        name = self.name
        name = name.lower()

        # Increment the fill index

        output =  "// Fill %s\n"%name
        output += '++%s._index;\n' % self.name;
        for i,var in enumerate(self.var_list):

            val = self.val_list[i].lower()
            com = self.com_list[i]

            # TODO: support strings
            # TODO: fix narrowing
            # Detect and handle multidim arrays
            if ';' in val:

                output += '{\n'
                val = val.strip(';') # remove trailing semi
                val = val.replace( '{','{{' )
                val = val.replace( '}','}}' )
                val = val.replace( ';', '},{')
                output += '%s_info::%s::Type temp = %s;\n'%(name,var,val)
                output += 'memcpy(&%s.%s, &temp, sizeof(temp));;\n'%(name,var)                
                output += '}\n'            
                
            elif '{' in val:

                # Convert all elements of array to floating point
                val = val.strip('{')
                val = val.strip('}')
                newval = []
                for v in val.split(','):
                    #newval.append('float(%s)'%v)
                    convert = '%s_info::%s::BaseType(%s)'%(name,var,v)
                    newval.append( convert )
                    
                val = '{\n' + ',\n'.join(newval) + '\n}\n'
                    
                output += '{\n'
                output += '%s_info::%s::Type temp = %s;\n'%(name,var,val)                
                output += 'memcpy(&%s.%s, &temp, sizeof(temp));;\n'%(name,var)                
                output += '}\n'
                
            else :
                output += '%s.%s = %s;\n'%(name,var,val)

            output += '// %s_docum.%s = "%s";\n'%(name,var,com)

        output += "%s.fill();\n"%name.upper()
        #print output

        document.impl( output, unit=current )


class Use(Handler):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):

        struct   = attr.get('struct').lower()
        selector = attr.get('select', None); 
        value    = attr.get('value',  None);

        if (selector):
            selector=selector.lower()
        if (value):
            value=value.lower()
        
        if ( selector == None ):
            selector = '_index'
            value    = '1'

        struct   = struct.lower()
        selector = selector.lower().strip()
        value    = replacements(value)    # May contain struct definitions

        type=_struct_table[ struct ][ selector ]
        if ( type == 'char' ):
            value = '"%s"'%value
        
        document.impl( '',                                       unit=current )
        document.impl( '/// USE %s %s=%s;'%(struct,selector,value), unit=current )
        if selector == '_index':            
            document.impl( '%s.Use();'%struct, unit=current )
        else:
            type = _struct_table[struct][selector]
            document.impl( '%s.Use("%s",(%s)%s);'%(struct,selector,type,value), unit=current )
        document.impl( '',                                       unit=current )
class Using(Handler):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):

        struct   = attr.get('struct').lower()
        selector = attr.get('select', None); 
        value    = attr.get('value',  None);

        if selector == None:
            selector = "_index"
            value    = "1"

        selector = selector.lower().strip()
        value    = value.lower().strip()
        value    = replacements(value)

        # TODO: handle strings
        document.impl( '/// USE %s %s=%s;'%(struct,selector,value), unit=current )
        document.impl( '%s.use( __%s_%s__ = %s );\n'%(struct.upper(), struct.lower(), selector.lower(), value ), unit=current)
        


# ----------------------------------------------------------------------------------------------------
        
def ag_variable( name ):
    name = name.lower()

    MATERIAL      =   CaselessKeyword("material")
    MATERIAL_VARS = ( CaselessKeyword("dens") |
                      CaselessKeyword("a")    |
                      CaselessKeyword("z")    |
                      CaselessKeyword("radl") |
                      CaselessKeyword("absl") |
                      CaselessKeyword("isvol") )

    MATERIAL_MATCH = ( MATERIAL      ('cmd') + '::' +
                       MATERIAL_VARS ('var') +
                       restOfLine    ('rest')
                       )

    try:
        result = MATERIAL_MATCH.parseString(name)
        cmd    = result.get('cmd',None)
        var    = result.get('var',None)
        rol    = result.get('rest',None)

        if rol:
            return '(_%s.par("%s"))'%(cmd,var) + rol
        else:
            return '(_%s.par("%s"))'%(cmd,var)

    except ParseException:
        return name
    
# ----------------------------------------------------------------------------------------------------

class Material(Handler):

    def __init__(self):
        Handler.__init__(self)
        self.name = ""
        self.opts=[]

    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):

        name = attr.get('name')
        self.name = name

        # List of attributes known to Material tag
        mylist = ['a', 'z', 'dens', 'absl', 'radl',
                  'ifield','isvol','fieldm','tmaxfd',
                  'epsil','stemax','deemax','stmin' ] # and additional medium parameters

        # Check validity of attributes and issue warning if we are provided
        # an unknown attribute
        checkAttributes( tag, attr, mylist, ['name'] )
                            
        # Build up the string for the material documentation
        doxy = 'Material %s '%name
       
        # Count the number of parameters specified in the material
        count = 0
        for a in mylist:
            if a=='name': continue
            opt = attr.get(a,None)                                                   
            if ( opt != None ):
                var = ag_variable(opt)
                self.opts.append( 'mat.par("%s")=%s;'%(a,var) )
                doxy = doxy + '%s=%s '%(a,var)
                count+=1        

        document.impl('/// %s'%doxy, unit=current )

        if ( count ):
            document.impl( '{ AgMaterial &mat = AgMaterial::Get("%s");' % camelCase(name),  unit=current )
            for p in self.opts:
                p = replacements(p) # May contain structure definitions
                document.impl( p, unit=current )
        else:
            document.impl( '{  AgMaterial mat = AgMaterial::CopyMaterial("%s");' % camelCase(name), unit=current )

                
    def endElement(self,tag):
        document.impl( '_material = mat;', unit=current )
        document.impl( '}', unit=current )
class Medium(Handler):

    def __init__(self):
        self.name = 0
        self.opts = []        
        Handler.__init__(self)
    def setParent(self,p): self.parent = p    

    def startElement(self,tag,attr):
        name = attr.get('name')
        self.name = name
        mylist    = ['ifield','fieldm','tmaxfd','epsil','stemax','deemax','stmin','isvol']

        checkAttributes( tag, attr, mylist, ['name'] )

        count = 0
        document.impl( '/// Medium %s'%name, unit=current )
        for a in mylist:
            opt = attr.get(a,None)
            if ( opt != None ):
                document.impl( '///  %s = %s'%(a,opt), unit=current )
                self.opts.append( 'med.par("%s")=%s;'%(a,opt) )
                count+=1
                
        if ( count ):
            document.impl( '{  AgMedium &med = AgMedium::Get("%s");' % camelCase(name), unit=current )
            document.impl( '   med.Inherit(this);',                                    unit=current ) 
            # Can we refactor this into AgMedium::Get(...)?  Ditto for AgMaterial?
            for p in self.opts:
                p = replacements(p)
                document.impl( p, unit=current )
                #out(unit='impl', text=p)
                
        else:
            document.impl( '{  AgMedium med = AgMedium::CopyMedium("%s");' % camelCase(name), unit=current )
                
    def endElement(self,tag):
        document.impl( '_medium = med;', unit=current )
        document.impl( '}', unit=current )
class Mixture(Handler):

    def setParent(self,p): self.parent = p    
    def __init__(self):
        self.name = ""        
        self.opts = []
        self.comps = []
        self.elements = []
        Handler.__init__(self)                    
    def addComponent(self, comp, element ):
        self.comps.append(comp)
        self.elements.append(element)
    def startElement(self,tag,attr):
        self.name = attr.get('name')
        mylist = [ 'a', 'z', 'isvol', 'dens', 'nc', 'radl', 'absl' ] # ADDED radl, absl 02/16/11

        checkAttributes( tag, attr, mylist, ['name'] )
        
        doxy = 'Mixture %s'%self.name
        for a in mylist:
            opt = attr.get(a,None)            
            if ( opt != None ):
                var = ag_variable(opt)
                self.opts.append( 'mix.par("%s")=%s;'%(a,var) )
                doxy += ' %s=%s'%(a,var)
        self.doxy = doxy
                
    def endElement(self,tag):
        document.impl( '/// %s'%self.doxy, unit=current )
        
        
        document.impl( '{  AgMaterial &mix = AgMaterial::Get("%s");' % camelCase(self.name), unit=current )
        for c in self.comps:
            c = replacements(c)
            document.impl( c, unit=current )
        for o in self.opts:
            o = replacements(o) # May contain structures
            document.impl( o, unit=current )

        document.impl( 'mix.lock();', unit=current )
        document.impl( '_material = mix;', unit=current )
        document.impl( '_material.lock();', unit=current )
        document.impl( '}', unit=current )

        self.checkComponents()

    def checkComponents(self):
        sumw = 0.0
        isMixt = False # Is a mixture IF one w < 1
        isComp = False # Is a compound IF one w > 1

        for ele in self.elements:
            w = 0.0
            try:                          w = float(ele['w'])                
            except ValueError:            w = eval(ele['w'])
            except:
                # May depend on variables, so pass through and catch at run time
                return
            sumw += w
            if w<1.0:                     isMixt=True
            if w>1.0:                     isComp=True            
            if float(int(w)) != float(w): isMixt=True

        if isMixt and isComp:
            RaiseException( MixtureComponentError( self ) )

        if sumw==100.0:
            RaiseWarning( MixtureComponentError( self ) )
class Component(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement( self, tag, attr ):
        name = attr.get('name')

        # Check validity of attributes and issue warning if we are provided
        # an unknown attribute
        mylist = ['a','z','w','A','Z','W']
        checkAttributes( tag, attr, mylist, ['name'] )        

        
        a    = attr.get('A')
        if ( a==None ): a = attr.get('a')
        z    = attr.get('Z')
        if ( z==None ): z = attr.get('z')
        w    = attr.get('W')
        if ( w==None ): w = attr.get('w')

        a=a.lower()
        z=z.lower()
        w=w.lower()
        
        self.parent.addComponent( 'mix.Component("%s",%s,%s,%s);'%( name, a, z, w ), {'name':name, 'a':a, 'z':z, 'w':w} )
        document.impl( '/// Component %s\ta=%s\tz=%s\tw=%s'%(name,a,z,w), unit=current )
class Attribute(Handler):

    def __init__(self):
        self.style = []
        self.name=""
        Handler.__init__(self)
    def setParent(self,p): self.parent = p    
    def startElement(self,tag,attr):
        self.name = attr.get('for')
        self.cond = attr.get('cond',None)

        checkAttributes( tag, attr, _agstar_attribute_list, ['for','cond'] )       
        
        for a in _agstar_attribute_list:
            v = attr.get(a,None)
            if v:
                v = v.lower()
                self.style.append( 'attr.par("%s")=%s;'%( a, v ) )

    def endElement(self,tag):

        # Add conditional 
        if self.cond:
            self.cond = replacements(self.cond).lower()
            document.impl( 'if ( %s )'%self.cond, unit=current )
        
        document.impl( '{ AgAttribute attr = AgAttribute("%s");'%self.name, unit=current )
        for a in self.style:
            a = replacements(a)
            document.impl( a, unit=current )
        document.impl( 'attr.Inherit( AgBlock::previous() ); ', unit=current ) # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
        document.impl( '_attribute = attr;', unit=current )
        document.impl( '}', unit=current )


def pconMap( name ):
    map = { 'zi' : 'Z',
            'rmn' : 'Rmin',
            'rmx' : 'Rmax' }
    return map[name]

def reShape( name ):
    reshape = { 'box': 'bbox' }
    try:
        return reshape[ name ]
    except KeyError:
        return name

###############################################################################
#
# TODO: Difference in AgROOT and AgSTAR creation  
#
# In AgROOT, we insert an if statement immediately after the <Shape /> tag which 
# checks whether the shape is the same as the last iteration.  If so, then it 
# returns at that point, skipping the rest of the user's <Block> ... </Block>.
# The AgSTAR interface behaves differently.  On Create, it jumps to the user's 
# BLOCK ... ENDBLOCK code, and creates the volume in the SHAPE call.  However,
# it continues to execute the code after the SHAPE command, even if the shape
# is the same.  In order to avoid filling with daughters multiple times, there
# is an explicit check which prevents the same object from being placed in the
# same position with the same rotation.
#
# My approach moves this check much earlier in the code.
#
###############################################################################

def shapeTypeReplacements( mytype ):

    if mytype=='npdv': mytype='npdiv'
    return mytype

class Shape(Handler):

    def __init__(self):
        Handler.__init__(self)
        self.arglist = []        
    def setParent(self,p):        self.parent = p    

    def startElement(self,tag,attr):

        # Get the shape's type
        mytype = shapeTypeReplacements( attr.get('type',None).lower() )

        # Do shape lookup and raise an exception if the shape is not implemented
        args = None
        try:
            args   = Dyson.Utils.Shapes.arglist( mytype )
        except:
            RaiseWarning( AgmlShapeError( current_block, mytype ) )

        # Check validity of attributes and issue warning if we are provided
        # an unknown attribute
        checkAttributes( tag, attr, args, ['type'] )        
        
        mytype = reShape(mytype)
        
        # Trigger the creation of a new AgShape container
        # and set the user-defined parameters

        doxy_shape = 'Shape %s '%camelCase(mytype)
        doxy_slice = {}

        document.impl( '{  AgShape shape = AgShape("%s");'% camelCase(mytype), unit=current )

        # Inherit shape, medium, material parameters from the previous
        # block to the extent that they have not yet been set
        document.impl('shape     .Inherit( AgBlock::previous() );', unit=current )

        # Finally, set any paramters which haven't been set already using
        # information passed in from Create
        document.impl('create     .SetParameters(shape);', unit=current );
        
        # Set user-defined arguements from the SHAPE operator
        for a in args:
            val = attr.get(a)
            if ( val == None ): continue
        
            # Detect the section arguments of p*ons
            if  re.match('\{',val):
                val = val.strip('{}')
                arr = val.split(',')
                doxy_slice[ pconMap(a) ] = arr
                for i,b in enumerate(arr):
                    b = b.lower()
                    b = replacements(b)
                    document.impl( 'shape.%s(%i)=%s;'%( pconMap(a),i,b), unit=current )                                        
                    
            else:
                val = val.lower()
                val = replacements(val)
                document.impl( 'shape.par("%s")=%s;'%(a,val), unit=current )
                doxy_shape += '%s=%s '%(a,val)

        document.impl( '/// %s' % doxy_shape, unit=current )
            

        document.impl( '_same_shape &= _stacker->SearchVolume( shape, _attribute );', unit=current )
        document.impl( '_shape = shape;', unit=current )
            
        document.impl( 'if (_same_shape) goto END_OF_%s;'%(current_block), unit=current )
        
        document.impl('_stacker -> Build(this);', unit=current )
        document.impl('}', unit=current )
class Create(Handler):

    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p        
    def startElement(self,tag,attr):
        """
        CREATE operator can accept a large number of attributes.  These attributes
        will be copied into an instance of the AgCreate class to be passed in to
        the block's Create method
        """
        block = attr.get('block')
        keys=[]
        for key,value in attr.iteritems():
            keys.append(key)


        checkAttributes( tag, attr, shape_params, ['block'] )
        requireAttributes( tag, attr, ['block'], warning=False )


        shape = {}
        count = 0
        for key in shape_params:
            val = attr.get(key,None)
            if ( val ):
                shape[key] = replacements( val.lower() )
                count+=1

        document.impl('_create = AgCreate("%s");'%block, unit=current );
        if count:
            document.impl('{ // Paramters passed in via the Create operator', unit=current );            
            document.impl('AgCreate createin("%s");'%block, unit=current )
            for key,value in shape.iteritems():
                document.impl('createin.par("%s")=%s;'%(key,value), unit=current)
            document.impl('_create = createin;', unit=current )                
            document.impl('}', unit=current );


        document.impl( '{', unit=current );
        document.impl( 'AgShape myshape; // undefined shape',unit=current )
        for key,value in shape.iteritems():
            value=value.lower()
            value=replacements(value)
            document.impl( '/// Set shape par: %s = %s'%(key,value), unit=current )
            document.impl( 'myshape.par("%s")=%s;'%( key,value ), unit=current )

        document.impl( '///Create %s'%block, unit=current )
        document.impl( 'Create("%s"); '%(block), unit=current )
        document.impl( '}', unit=current );



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
# >>>> TODO <<<<
    def __init__(self):
        self.pos  = []
        self.into = None
        self.block = None
        self.form  = None #Formatter()
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

        # Check validity of attributes and issue warning if we are provided
        # an unknown attribute
        for key,value in attr.iteritems():
            if key=='block': continue
            if key=='into' : continue
            key = key.lower()
            if key in mylist:
                pass
            else:
                #warn(  AgmlAttributeWarning( current_block, tag, key, value ) )
                RaiseWarning( AgmlAttributeWarning( current_block, tag, key, value ) )
        
        for key in mylist:
            att = attr.get(key,None)        
            if ( att != None ):
                self.pos.append( "%s=%s"%( key,att ) )
        only=attr.get('konly',None)
        if ( only != None ):
            only=only.strip("'")
            self.pos.append( "%s='%s'" %( "konly", only ) )
    def endElement(self,tag):
        output  = "POSITION %s " %self.block
        if ( self.into != None ):
            output +=" in %s"%self.into
        #last=len(self.pos);
        for pos in self.pos:
            pos=pos.strip('\n') # chomp
            pos=pos.strip(',')  # strip stray commas from arguements
            output += " %s"% pos
class Create_and_Position(Position):
# >>>> TODO <<<<
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
class Placement(Handler):

    def __init__(self):
        Handler.__init__(self)
        self.contents = []
    def setParent(self,p): self.parent=p

    def startElement(self,tag,attr):

        # Positional arguements
        block = attr.get('block')        
        into  = attr.get('in',None)
        group = attr.get('group',None)
        x     = attr.get('x',None)
        y     = attr.get('y',None)
        z     = attr.get('z',None)
        only  = attr.get('konly',None)
        copy  = attr.get('ncopy',None)
        cond  = attr.get('if',None)   # conditional placement
        matrix= attr.get('matrix',None)

        table = attr.get('table',None) # DB table
        row   = attr.get('row','0')    # row


        if cond:
            cond = replacements(cond)
            cond = cond.lower()
            
        self.cond = cond

        # Validate attributes
#       checkAttributes( tag, attr, ['in','x','y','z','konly','ncopy'], ['block'] )  # plus shape stuff
        requireAttributes( tag, attr, ['block'], warning=False )

        if into==None:

            # Navigate up the xml stack until we find the parent
            # block or module, then set into accordingly.
            
            parent = self.parent
            while parent!= None:

                if parent.__class__==Block:
                    into = parent.name
                    break

                if parent.__class__==Module:
                    into = 'CAVE'
                    break

                parent = parent.parent

        if into==None:
            print 'AgROOT Warning: parent of block %s not in stack WTF?' % block

        


        self.attr  = attr

        block = replacements(block) # unlikely
        into  = replacements(into)  # unlikely
        if into == 'Cave': into = 'CAVE'
        if x: x=x.lower()
        if y: y=y.lower()
        if z: z=z.lower()
        x = replacements(x)
        y = replacements(y)
        z = replacements(z)
        if only: only = replacements(only.strip("'"))
        copy = replacements(copy)

        self.block = block
        self.into  = into
        self.pos   = [x, y, z, only, copy]

        # Convert matrix into a nested list of elements
        if matrix:
            matrix = parseArray(matrix)

        # If provided, add the conditional to the placement
        if cond:            document.impl( 'if ( %s )'%cond, unit=current )

        document.impl( '{', unit=current ) # Open scope
        if oldplacement:
            document.impl( 'AgPlacement place;', unit=current )
        else:
            document.impl( 'AgPosition  place;', unit=current )
        document.impl( 'place.SetBlock("%s");'%block, unit=current )
        document.impl( 'place.SetMother("%s");'%into, unit=current )
        if group:
            document.impl( 'place.SetGroup("%s");'%group, unit=current )
            
        document.impl( '/// Add daughter volume %s to mother %s'%(block,into), unit=current )
        
        if ( x != None ):
            document.impl( 'place.TranslateX(%s);'% x, unit=current )
            document.impl( '/// Translate x = %s'%x, unit=current )
        if ( y != None ):
            document.impl( 'place.TranslateY(%s);'% y, unit=current )
            document.impl( '/// Translate y = %s'%y, unit=current )
        if ( z != None ):
            document.impl( 'place.TranslateZ(%s);'% z, unit=current )
            document.impl( '/// Translate z = %s'%z, unit=current )

        if ( matrix != None ):
            array = ""
            for element in matrix:
                array += '{%s},'%','.join(element)
            array = array.rstrip(',')
            # Matrix assignment 
            document.impl( '{ double matrix[4][4] = {%s}; place.SetOrder( AgPosition::kGeneral ); place.Matrix( matrix ); }'%array, unit=current )
            document.impl( '/// Rotation Matrix = %s'%array, unit=current )


        if ( table != None ):
            chair = table.split('/')[-1]
            
            document.impl( 'place.SetTable("%s",%s); // Use DB table to position object (if available)'%(table,row), unit=current )
            document.impl( 'place.SetChair("%s");    // Use DB table to position object (if available)'%(chair    ), unit=current )            

        
        if ( only != None ):
            document.impl( 'place.par("only")=%s;'% only, unit=current )
            document.impl( '/// Overlap: %s'%only.lower(), unit=current )            
        if ( copy != None ):
            document.impl( 'place.par("ncopy")=%s;'% copy.lower(), unit=current )
            document.impl( '/// Ncopy: %s'%copy.lower(), unit=current )

        # Append shape parameters
        for key in shape_params:
            val = attr.get(key,None)
            if val:
                val = val.lower()
                val = replacements(val)
                document.impl( 'place.par("%s")=%s;'%( key, val ), unit=current )

    def endElement(self,tag):

        document.impl( '_stacker -> Position( AgBlock::Find("%s"), place );'%( self.block ), unit=current )
        document.impl( '} // end placement of %s' %self.block, unit=current )
        

    def add(self,thingy):
        self.contents.append(thingy)
class Translation(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        x=attr.get('x',None)
        y=attr.get('y',None)
        z=attr.get('z',None)

        # Validate attributes
        checkAttributes( tag, attr, ['x','y','z'] )
        
        if ( x ) : document.impl('place.TranslateX(%s);' % replacements(x.lower()) , unit=current )
        if ( y ) : document.impl('place.TranslateY(%s);' % replacements(y.lower()) , unit=current ) 
        if ( z ) : document.impl('place.TranslateZ(%s);' % replacements(z.lower()) , unit=current )        


class Rotation(Handler):

    def __init__(self):
        Handler.__init__(self)
        self.key    = None
        self.value  = None
        self.angles = None

    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):

        # Validate attributes
        checkAttributes( tag, attr, ['matrix','alphax','alphay','alphaz','thetax','thetay','thetaz','phix','phiy','phiz','ort'] )

        list = ['alphax','alphay','alphaz' ]
        func = {'alphax' : 'AlphaX',
                'alphay' : 'AlphaY',
                'alphaz' : 'AlphaZ'}

        for key in list:
            val = attr.get(key)
            if ( val != None ):
                val = val.lower()
                val = replacements(val) #$$$
                document.impl( 'place.%s(%s);' %( func[key], val ), unit=current )
                document.impl( '/// Rotate: %s = %s'%(func[key],val), unit=current )

        self.angles = {}
        list = ['thetax','phix',
                'thetay','phiy',
                'thetaz','phiz']

        defs = { 'thetax': 90, 'phix':  0,
                 'thetay': 90, 'phiy': 90,
                 'thetaz':  0, 'phiz':  0 }

        stmt = 'Double_t '
        count = 0
        for i,key in enumerate(list):
            val = attr.get(key,None)
            sep = ','
            if (i==len(list)-1): sep=';'

            if  val != None:
                val = val.lower()
                val = replacements(val)
                self.angles[key] = attr.get(key)
                stmt += '_'+ '%s=%s%c'%(key,val,sep)
                document.impl( '/// G3 Reference: %s = %s'%(key,val), unit=current )
                
                count += 1
                # Count number of non default keys
            else:
                self.angles[key] = defs[key]
                stmt += '_' + '%s=%s%c'%(key, defs[key], sep)
                document.impl( '/// G3 Reference: %s = %s'%(key,defs[key]), unit=current )                
                # No increment for default

        if count:
            document.impl(  stmt, unit=current )
            document.impl( 'place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );', unit=current)

        ortho = attr.get('ort',None)
        if ( ortho ):
            document.impl( 'place.Ortho( "%s" ); // ORT=%s'%( ortho, ortho ), unit=current )
            document.impl( '/// Axis substitution: XYZ --> %s'%ortho, unit=current )
     
        matrix = attr.get('matrix',None)
        if matrix:
            document.impl( '{ // rotation matrix (overwrites any prior rotation)', unit=current )
            document.impl( '  double _rotm[] = %s;'%matrix,                        unit=current )
            document.impl( '  place.RotationMatrix( _rotm );',                     unit=current )
            document.impl( '}',                                                    unit=current )

               
class Misalign(Handler):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent = p
    def startElement(self,tag,attr):        
        matrix = attr.pop('matrix',None) # get transformation matrix
        table  = attr.pop('table', None) # alternatively get db table
        row    = attr.pop('row',    '0') # ... and row of table
        opts   = attr.pop('opts','+xyzr')
        document.impl( 'place.Misalign("%s",%s,"%s");'%(table,row,opts), unit=current )
    def endElement(self,tag):
        pass
        
# ----------------------------------------------------------------------------------------------------
class For(Handler):

    def setParent(self,p):     	self.parent = p
    def __init__(self): 
    	Handler.__init__(self)                    
    	self.var = ''

    def startElement(self,tag,attr):

        checkAttributes( tag, attr, ['var','from','to','step'] )
        
        variable = attr.get('var',None)  .lower()
        myfrom   = attr.get('from',None) .lower()
        to       = attr.get('to',None)   .lower()
        step     = attr.get('step','1')  .lower()


        variable = replacements(variable)
        myfrom   = replacements(myfrom)
        to       = replacements(to)
        step     = replacements(step)
        
        document.impl( '/// Loop on %s from %s to %s step=%s'%(variable,myfrom,to,step), unit=current )
        document.impl( 'for ( %s=%s; (%s>0)? (%s<=%s):(%s>=%s); %s+=%s )'%(variable,myfrom,step,variable,to,variable,to,variable,step), unit=current )
        document.impl( '{', unit=current )
        
        self.var = variable

    def endElement(self,tag):
        document.impl('}', unit=current)

    def characters(self,content):
        content = content.lstrip()
        content = content.rstrip()
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
             content += ';'
             document.impl( content, unit=current )
class While(Handler):

    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)
    def startElement(self,tag,attr):

        checkAttributes( tag, attr, ['expr'] )
        
        expr=attr.get('expr')
        expr = replacements( expr.lower() )
        document.impl( 'while ( %s )'%expr, unit=current )
        document.impl( '{', unit=current )

    def endElement(self,tag):
        document.impl( '}', unit=current )

    def characters(self,content):
        content = content.lstrip()
        content = content.rstrip()
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
             content += ';'
             document.impl( content, unit=current )
class Foreach(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)
class Call(Handler):
    
    def setParent(self,p): self.parent = p
    def __init__(self):
        Handler.__init__(self)

    def startElement(self,tag,attr):        
        self.routine = attr.get('routine')
        self.expr    = attr.get('expr')
        self.expr    = replacements( self.expr.lower() )
        self.expr    = self.expr.replace("'",'"') # from single to double quotes

        if self.routine.lower() in routine_skip_list:            document.impl('/*{', unit=current )        
        #form('Call %s( %s )' %( self.routine, self.expr ) )
        document.impl( '%s( %s );// CALL %s'%(self.routine,self.expr,self.routine), unit=current )
        if self.routine.lower() in routine_skip_list:            document.impl('}*/', unit=current )
class If(Handler):

    def setParent(self,p): self.parent = p    
    def __init__(self):
        Handler.__init__(self)
        self.expr=0

    def startElement(self,tag,attr):

        requireAttributes( tag, attr, ['expr'], warning=False )
        
        expr = attr.get('expr')
        expr = replacements( expr.lower() )        
        self.expr=expr
               
        document.impl( '', unit=current )
        document.impl( 'if ( %s )' % expr, unit=current )
        document.impl( '{', unit=current )

    def endElement(self,tag):
        document.impl( '}', unit=current )
        document.impl( '',  unit=current )
                
    def characters(self,contents):
        # Handle multiple commands sep by ;
        for content in contents.split(';'):
            content = content.lstrip()
            content = content.rstrip()
            content = content.lower()
            content = replacements(content)        

            if ( not re.match(';$',content) ):
                content += ';'

            document.impl( content, unit=current )
#   def characters(self,content):        MAIN differs from AgML100417 in For/Do loops... keep MAIN
#       content = content.lstrip()
#       content = content.rstrip()
#       content = content.lower()
#       content = replacements(content)        
#       if ( not re.match(';$',content) ):
#           content += ';'
#       document.impl( content, unit=current )
                
class Then(Handler): # always ignoring thens
    def setParent(self,p):
        self.parent = p
    def __init__(self):
        Handler.__init__(self)    
class Elif(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self):
        Handler.__init__(self)
    def startElement(self,tag,attr):

        requireAttributes( tag, attr, ['expr'], warning=False )
        
        expr = attr.get('expr')
        expr = replacements( expr.lower() )
        self.expr = expr
                
        document.impl( '}', unit=current ) # close preceding if or elif block
        document.impl( 'else if ( %s )' % expr, unit=current )
        document.impl( '{', unit=current )
        
    def endElement(self,tag):
        pass # allow the </If> or the next <Elif> or <Else> to close the block

    def characters(self,content):
        content = content.lstrip()
        content = content.rstrip()
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
             content += ';'
        document.impl( content, unit=current )
class Else(Handler):
    def setParent(self,p): self.parent = p    
    def __init__(self): Handler.__init__(self)

    def startElement(self,tag,attr):
        document.impl( '}',    unit= current) # close preceding block
        document.impl( 'else', unit= current)
        document.impl( '{',    unit= current)

    def endElement(self,tag):
        pass # allow the </If> to close the block

    def characters(self,content):
        content = content.lstrip()
        content = content.rstrip()
        content = content.lower()
        content = replacements(content)        
        if ( not re.match(';$',content) ):
             content += ';'
        document.impl(content, unit=current)
class Check(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p
    def startElement(self,tag,attr):
        expr=attr.get('expr',None)        
        if expr:
            expr = expr.lower()
            expr = replacements(expr)            
            if self.parent.__class__ == Block:
                document.impl( 'if ( not (%s)) { return;   }'%expr,unit=current)
            else:
                document.impl( 'if ( not (%s)) { continue; }'%expr,unit=current)

class Author(Handler):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):

        checkAttributes( tag, attr, ['name','email'] )
        
        name = attr.get('name')
        email = attr.get('email',None)
        document.impl('///@addtogroup %s_revision'%document.agmodule, unit=current )
        document.impl('///@{', unit=current )        
        document.impl('/// Author: %s'%name, unit=current )        
        if ( email != None ):
            document.impl('/// Email:  %s'%email, unit=current )
        document.impl('///@}', unit=current )            
class Created(Handler):

    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):

        checkAttributes( tag, attr, ['date'] )

        date = attr.get('date')
        document.impl('///@addtogroup %s_revision'%document.agmodule, unit=current )
        document.impl('///@{', unit=current )
        document.impl('/// Created: %s'%date, unit=current )
        document.impl('///@}', unit=current )        
class Translator(Handler):
# TODO            
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        name = attr.get('name')
        email = attr.get('email',None)
        if ( email != None ):
            #form( "!Translator %s <%s>"%( name, email ) )
            pass
        else:
            #form( "!Translator %s"%name )
            pass

class Par(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):
        name   = attr.get('name')
        value  = attr.get('value').strip()
        value = replacements(value).lower()

        requireAttributes( tag, attr, ['name','value'] )
        
        #document.impl( '// _medium.par("%s") = %s;'%(name,val), unit=current )
        document.impl( '// parameter %s = %s (deferred)'%(name.lower(),value.lower()), unit=current )        
class Cut(Handler):
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    def startElement(self,tag,attr):

        requireAttributes( tag, attr, ['name','value'] )
        
        name   = attr.get('name')
        value  = attr.get('value')

        #document.impl( '// _medium.par("%s") = %s;'%(name,val), unit=current )
        document.impl( '// cut %s = %s (deferred)'%(name.lower(),value.lower()), unit=current )
class Hits(Handler):
# TODO        
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
                 'eta', 
                 'slen', 'tof', 'step',
                 'sleng', 'lptot',
                 'birk', 'eloss', 'elos',
                 'user', 'etsp', 'ptot', 'lgam' ]

        # Issue warning for HITS which are not in the above list
        # as a reminder that certain detectors need to implement
        # digitization routines
        checkAttributes( tag, attr, list+['for','type'], warning=True )
        
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
class Hit(Handler):
    def __init__(self):
        Handler.__init__(self)
    def setParent(self,p):
        self.parent=p
    def startElement(self,tag,attr):
        self.attr = attr
        self.parent.addHit(self)
    def endElement(self,tag):
        pass
class Instrument(Handler):
    """
    Instrument is processed in scope of a block
    """
    def __init__(self):
        Handler.__init__(self)
        self.hit_list = []
    def setParent(self,p):
        self.parent = p
    def addHit(self,hit):
        self.hit_list.append(hit)
    def startElement(self,tag,attr):
        self.block = attr.get('block', attr.get('volume', None))        
    def endElement(self,tag):
        block = self.block
        for hit in self.hit_list:
            attr = hit.attr
            meas = attr.get('meas',None)
            nbits=attr.get('nbits', attr.get('bins', '0') )
            mn   = attr.get('min','0')
            mx   = attr.get('max','0')
            opts = attr.get('opts','')
            # apply replacement table to calculated(-able) quantities
            nbits=replacements(nbits).lower()
            mn   =replacements(mn).lower()
            mx   =replacements(mx).lower()
            document.impl( '// deferred module()->AddHit( "%s", "%s", %s, %s, %s, "%s");'%( block, meas, nbits, mn, mx, opts ), unit=current )            


class Gsckov(Handler):
# TODO            
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    pass
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
            ele = ele.lower()
            self.args.append(ele)

    def descriptors(self):
        array  = self.format.split('{')
        mylist = ""

        # Loop over all elements
        for element in array:

            # If the element is a format descriptor
            if '}' in element:
                # Strip the right bracket
                element=element.strip('}')
                # Get the last character
                last = element[ len(element) - 1 ].lower()

                assert( last in "idf" )
                
                # Get the rest
                rest = element[ 0 : len(element) - 1 ]
                # And append the descriptor to the list
                mylist += "%%%s%s "%(rest,last)

            else:
                #element = element.lower()
                #element = replacements(element)
                mylist += element



        return mylist

    def XXXendElement(self,tag):
        # Get the list of descriptors
        desc = self.descriptors()
        # Add to the Form...
        output = 'std::cout << Form("%s",'%desc                
        for i,a in enumerate(self.args):
            a = a.strip()
            if i and i<len(self.args): output += ' ,'
            output += '%s'%a
        output = replacements(output)            
        output += ') << std::endl;'
        document.impl(output,unit=current)

    def endElement(self,tag):
        # Get list of format descriptors and build the format line
        desc = self.descriptors()
        form = 'Form( "%s", '%(desc)
        for i, a in enumerate(self.args):
            a = a.strip()
            if i and i < len(self.args): form += ' ,'
            form += '%s'%a
        form = replacements(form)
        form += ') '
        
        document.impl( "// - Info - ",                    unit=current )
        document.impl( 'Info( GetName(), %s );'%form,     unit=current )
class Print(Handler):

    def __init__(self):
        self.level  = 0
        self.format = 0
        self.args   = []        
        Handler.__init__(self)
    def setParent(self,p): self.parent = p

    def startElement(self,tag,attr):
        self.level  = attr.get('level','1')
        self.format = attr.get('fmt', None)

    def characters(self,content):
        content = replacements(content)        
        if ( len(content.strip()) > 0 ):
            self.args.append(content)


    def endElement(self,tag):

        # Detect I/O format mode
        count=0
        for c in self.format:
            if c=='%': count+=1

        if not count:
            document.impl('// Print<level=%i> fmt=%s fortran format statements not supported', unit=current)
            return

        logger = 'LOG_INFO'
        if self.level>0:
            logger = 'LOG_DEBUG'

        if not len(self.args):
            document.impl( '%s << Form("%s") << std::endl;' %(logger, self.format), unit=current )
        else:
            stmt = self.args[0].strip()
            for i,a in enumerate(self.args):
                if i and len(a.strip()):
                    stmt += ', %s'  % a          
            document.impl( '%s << Form("%s",%s) << std::endl;' %(logger, self.format, stmt), unit=current )
class Replace(Handler):
# TODO            
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
##        content = replacements(content)        
        self.replace.append(content)
    def endElement(self,tag):
        if ( len(self.replace)==1 ):
            #form( "REPLACE [%s] with [%s];"%( self.match.rstrip(';'),
            #                                  self.replace[0].rstrip(';') ) );
            pass
        else:
            #form( "REPLACE [%s] with [" %self.match.rstrip(';') )
            for rpl in self.replace:
                rpl = rpl.rstrip(';')

class Function(Handler):
# TODO        
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
        content = replacements(content)        
    def endElement(self,tag):
        pass






# ====================================================================================================
class Fatal(Handler):
# TODO            
    def __init__(self): Handler.__init__(self)
    def setParent(self,p): self.parent = p        
    pass # but an exception is coming!
# ====================================================================================================

if __name__ == '__main__':


    if ( 1 ):
        print ag_variable( 'material::dens*vacuum')
    

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
       
