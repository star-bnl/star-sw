#!/usr/bin/env python

import cProfile;

from xml.sax import saxutils
from xml.sax import make_parser
from xml.sax.handler import feature_namespaces

from optparse import OptionParser

parser = 0

import sys
sys.path.append("$STAR/")
sys.path.append("./")

# Do not export bytecode to pyc files
sys.dont_write_bytecode = True

#import Dyson.Syntax.SyntaxHandler as SyntaxHandler
from Dyson.Syntax.SyntaxHandler import SyntaxHandler

#import Dyson.Syntax.SyntaxHandler
#dh=SyntaxHandler()
syntax = None

# Create a parser
parser = make_parser()

# Tell the parser we are not interested in XML namespaces
parser.setFeature(feature_namespaces, 0)

# Create a command line interpreter
#class AgParser(cmd.Cmd):

def main():

    global syntax

    optparser = OptionParser()
    optparser.add_option("--file",  help="Selects the geometry file to be parsed.", dest="filename", default="NONE")
    optparser.add_option("--export", help="Selects the export language (Mortran,AgML,...)", dest="language",default="AgML")
    optparser.add_option("--module", help="Defines the name of the module for export",      dest="module_name", default=None)
    optparser.add_option("--path",   help="Defines the path to export the files",           dest="path_name",   default='./geom/')
    optparser.add_option("--profile", help="Enables python profileing", dest="profiler", default = False, action = 'store_true' )
        
    (opts, args) = optparser.parse_args()

    # Create the requested syntax handler
    syntax = SyntaxHandler( opts )
    syntax_type = opts.language

    # Create the handler
    parser.setContentHandler(syntax)
    
    # Parse the document document
    #cProfile.runctx('parser.parse( opts.filename )')

    if opts.profiler:
        cProfile.runctx('parse(f)', { 'parse' : parser.parse, 'f' : opts.filename }, {} )
    else:
        parser.parse( opts.filename )
    

if __name__ == '__main__':
    main()
        
