#!/usr/bin/env python
import os
import sys
from optparse import OptionParser

import ROOT
ROOT.gErrorIgnoreLevel = 9999

def main():

   sys.path.append(os.environ['ROOTSYS'] + "/lib")
   sys.path.append("StarVMC/StarBASE/macros")
        

   #
   # Need to parse command line arguements first, because PyROOT is going 
   # to muck up the usage as soon as a root class is loaded.
   #
   parser = OptionParser()
   parser.add_option( "-b", "--base",   dest="baseline", help="Set the baseline geometry [required]",     metavar="BASE",   default="NONE" )
   parser.add_option( "-g", "--geom",   dest="geometry", help="Set the comparison geometry [required]",   metavar="GEOM",   default="NONE" )
   parser.add_option( "-v", "--volume", dest="volume",   help="Set the top level volume [required]",      metavar="VOLUME", default="CAVE" )
   
   (opts,args) = parser.parse_args()

   if ( opts.baseline == "NONE" ):
      print ""
      print "Must specify a baseline geometry."
      print ""
      os.system("./differential.py --help")
      return

   if ( opts.geometry == "NONE" ):
      print ""
      print "Must specify a comparison geometry."
      print ""
      os.system("./differential.py --help")
      return   
      

   from Differential import Differential, _file_path
   from Differential import get_geom_file
   from Canvas       import CanvasPDF

   from ROOT import TFile
   from ROOT import TGeoManager
   from ROOT import TGeoVolume
   from ROOT import TGeoNode

   from ROOT import kWhite
   from ROOT import gStyle

   gStyle.SetHistMinimumZero();
   gStyle.SetCanvasColor(kWhite); 
   
   canvas = CanvasPDF(    name="differential-"+opts.baseline+"-vs-"+opts.geometry+"-"+opts.volume,
                          title="Geometry differential for volume="+opts.volume+ " "+opts.baseline+" vs "+opts.geometry,
                          nx=1,
                          ny=1 
                          )

   differ = Differential( base=opts.baseline, 
                          comp=opts.geometry,
                          top=opts.volume,
                          canvas=canvas 
                          )




if __name__ == '__main__':
   main()
