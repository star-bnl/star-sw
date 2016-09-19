import sys

from  xml.sax.handler import ContentHandler

from xml.sax import saxutils
#from xml.sax import make_parser
#from xml.sax.handler import feature_namespaces

# Do I need this dependency?
from Dyson.Utils.IOHandler import IOHandler

#
# \class SyntaxHandler
# \author Jason C. Webb
#
# SyntaxHandler is responsible for translating the algorithmic
# portion of the AgXML Mortran into its Mortran Mortran.
#

# Language selection
import Dyson.Export
from   Dyson.Export import AgML
from   Dyson.Export import AgDL
from   Dyson.Export import AgROOT
from   Dyson.Export import Mortran

export_table = {
    'Mortran' : Mortran,
    'AgML'    : AgML,
    'AgDL'    : AgDL,
    'AgROOT'  : AgROOT
    }

Language = export_table['AgML'] # Default language

out = IOHandler();

def setExportSyntax ( syn ):
    """setExportLanguage( syn )

    Defines the export syntax.
    """
    global Language
    try:
        Language = export_table[syn]
    except KeyError:
        print """
        %s has not been defined as an export syntax.  Possibilities are:
        """ % syn

        for key,value in export_table.iteritems():
            print "%s --> %s"%( key, value )
        sys.exit(0)
    
#from Handler import Handler


class SyntaxHandler ( ContentHandler ):

    def depth(self):
        """
        Returns the current depth of the stack.
        """
        return len(self.stack)

    def setDocumentLocator(self,locator):
        self.locator = locator
        self.language.locator = locator


    def __init__(self,options):

        syn = options.language
        setExportSyntax(syn)

        self.language = Language

        self.options=options

        # This is the stack which handles each tag.  When the
        # parser encounters an open tag, the syntax handler
        # adds a new object to the stack.  When the end tag
        # is encountered, the object is popped off the stack.
        self.stack = [ None ]


        # Dictionary of classes which handle each tag defined
        # in the AgML (Abstract geometry mortran language).
        self.syntax  = {}
        
        #              AGML TAG      Class which handles tag
        # =================================================== # Document handling
        self.addToken( 'Document'  , Language.Document      )
        # ===================================================  
        self.addToken( 'Export'    , Language.Export        ) 
        # =================================================== # Detector modules
        self.addToken( 'Module'    , Language.Module        )   
        self.addToken( 'Block'     , Language.Block         )
        self.addToken( 'Volume'    , Language.Block         ) # AgML syntax
        self.addToken( 'Group'     , Language.Group         ) 
        self.addToken( 'Subroutine', Language.Subroutine    )
        self.addToken( 'Content'   , Language.Content       )
        self.addToken( 'Include'   , Language.Include       )
        self.addToken( 'CDE'       , Language.Cde           )
        self.addToken( 'Inline'    , Language.Inline        )   
        self.addToken( 'External'  , Language.External      )
        self.addToken( 'Import'    , Language.Import        )
        # ===================================================       
        self.addToken( 'var'       , Language.Var           )
        self.addToken( 'varlist'   , Language.Varlist       )
        self.addToken( 'Data'      , Language.Data          )
        self.addToken( 'Parameter' , Language.Parameter     )
        self.addToken( 'enum'      , Language.Enum          )
        self.addToken( 'Assign'    , Language.Assign        ) # New in AgROOT 
        self.addToken( 'Keep'      , Language.Keep          ) # New in AgROOT
        # ===================================================
#       self.addToken( 'Structure' , Language.Structure     )
        self.addToken( 'Structure' , Language.Struct        )     
        self.addToken( 'Struct'    , Language.Struct        ) # New style struct
#       self.addToken( 'Fill'      , Language.Fill          )
        self.addToken( 'Fill'      , Language.Filling       )
        self.addToken( 'Filling'   , Language.Filling       ) # New style struct
#       self.addToken( 'Use'       , Language.Use           )
        self.addToken( 'Use'       , Language.Using         )
        self.addToken( 'Using'     , Language.Using         ) # New style struct
        # ===================================================
        self.addToken( 'Shape'     , Language.Shape         )
        self.addToken( 'Material'  , Language.Material      )
        self.addToken( 'Medium'    , Language.Medium        )
        self.addToken( 'Mixture'   , Language.Mixture       )
        self.addToken( 'Component' , Language.Component     )
        self.addToken( 'Attribute' , Language.Attribute     )
        self.addToken( 'Create'    , Language.Create        )
        self.addToken( 'Position'  , Language.Position      )

        self.addToken( 'Placement',  Language.Placement     )
        self.addToken( 'Translation', Language.Translation  )
        self.addToken( 'Rotation',    Language.Rotation     )
        
        self.addToken( 'Create_and_Position',
                       Language.Create_and_Position         )
        # ===================================================
        self.addToken( 'Do'        , Language.For           ) 
        self.addToken( 'For'       , Language.For           )
        self.addToken( 'While'     , Language.While         )
        self.addToken( 'Foreach'   , Language.Foreach       )
        # ===================================================
        self.addToken( 'If'        , Language.If            )
        self.addToken( 'Then'      , Language.Then          )         
        self.addToken( 'Elif'      , Language.Elif          )
        self.addToken( 'Else'      , Language.Else          )
        self.addToken( 'Return'    , Language.Return        )
        self.addToken( 'Check'     , Language.Check         )
        # ===================================================
        self.addToken( 'Author'    , Language.Author        )
        self.addToken( 'Created'   , Language.Created       )
        self.addToken( 'Translator', Language.Translator    )
        self.addToken( 'Comment'   , Language.Comment       )     
        # ===================================================
        self.addToken( 'Gstpar'    , Language.Par           )
        self.addToken( 'Par'       , Language.Par           )
        #
        self.addToken( 'Hits'      , Language.Hits          )
        self.addToken( 'Instrument', Language.Instrument    )
        self.addToken( 'Hit'       , Language.Hit           )
        #
        self.addToken( 'Cut'       , Language.Cut           )
        self.addToken( 'Gsckov'    , Language.Gsckov        )
        # ===================================================
        self.addToken( 'Print'     , Language.Print         )
        self.addToken( 'Info'      , Language.Info          )
        # ===================================================
        self.addToken( 'Replace'   , Language.Replace       )
        # ===================================================        
        self.addToken( 'common'    , Language.Fatal         )
        self.addToken( 'goto'      , Language.Fatal         )
        self.addToken( 'format'    , Language.Fatal         )
        # ===================================================
        self.addToken( 'Call'      , Language.Call          )
        # ===================================================        
        self.addToken( 'Function'  , Language.Function      )
        self.addToken( 'Arguement' , Language.Arguement     )
        # =================================================== # Detector module configuration
        # Syntax elements for detector configuration
        self.addToken( 'Detector'  , Language.Detector      )
        self.addToken( 'Setup'     , Language.Setup         )
        self.addToken( 'Modules'   , Language.Modules       )
        self.addToken( 'Init'      , Language.Init          )
        # =================================================== # Master geomtery configuration
        self.addToken( 'StarGeometry', Language.StarGeometry )
        self.addToken( 'Tag'       , Language.Tag           )
        self.addToken( 'Geometry'  , Language.Geometry      )
        self.addToken( 'Construct' , Language.Construct     )                
        # ===================================================
        self.current = 0
        # ===================================================
        
    def addToken(self,token,handler):
        self.syntax[ token ] = handler;

    
    # On the start of any XML <TAG> ... </TAG> or <TAG />
    # we push a new handler onto the handler stack
    def startElement( self, tag, xattr ):
        """
        Place the handler on the stack and
        call it's start element method.  (Remember,
        'handler' here is the class ctor).
        """

        attr = {}                         # Copy to a plain dictionary
        for key in xattr.keys():
            attr[key] = xattr.get(key)

        if ( tag == 'Document' ):         # Special action: append command line options
            attr['cmdline']=self.options

        
        handler = None
        try:
            handler = self.syntax[ tag ]
        except KeyError:
            print ""
            print "================================================================="
            print "ERROR: The tag %s is unknown to the syntax handler" % tag
            print "       Please make sure the case is correct (i.e. 'camelcase',"
            print "       is different from 'camelCase' and 'CamelCase'.  If the"
            print "       case is correct, please email this message to the simu"
            print "       team:"
            print ""
            print "   --   print summary of AgML at this point    --"
            print "   -- print summary of XML stack at this point --"
            print ""
            print "================================================================="
            print ""            
            sys.exit(0) # wah wah wah waaaahhhhh
            return

        self.push( handler() ).startElement( tag, attr )
            
    def characters(self,content):
        """
        Call the character method on the handler which
        is at the top of the stack
        """
        self.top().characters( content )
            
    def endElement( self, tag ):
        """
        Pop the stack of handlers and call the
        end element on the top of the stack
        """        
        self.pop().endElement(tag)

    def push( self, h ):

        h.setParent( self.top() )
        self.stack.append(h)
        return self.top()
    
    def pop(self):
        return self.stack.pop()

    def top(self):
        return self.stack[ len(self.stack)-1 ]


# ===================================================================================
#def main():
#
#    syntax = SyntaxHandler()
#
#if __name__ == '__main__':
#    main()
