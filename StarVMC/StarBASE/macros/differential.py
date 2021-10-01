#!/usr/bin/env python
import os
import sys
from optparse import OptionParser

sys.path.append(os.environ['ROOTSYS'] + "/lib")
sys.path.append("StarVMC/StarBASE/macros")

import ROOT
ROOT.gErrorIgnoreLevel = 9999      # silence messages
ROOT.gROOT.SetBatch()              #

def main():

   #
   # Need to parse command line arguements first, because PyROOT is going 
   # to muck up the usage as soon as a root class is loaded.
   #
   parser = OptionParser()
   parser.add_option( "-b", "--base",   dest="baseline", help="Set the baseline geometry [required]",     metavar="BASE",   default="NONE" )
   parser.add_option( "-g", "--geom",   dest="geometry", help="Set the comparison geometry [required]",   metavar="GEOM",   default="NONE" )

   parser.add_option( "--basename", dest="basename",help="Set the name of the baseline geometry if different from base [optional]", metavar="BASENAME", default="same" )
   parser.add_option( "--geomname", dest="geomname",help="Set the name of the comparsion geometry if different from geom [optional]", metavar="GEOMNAME", default="same" )

   parser.add_option( "-v", "--volume", dest="volume",   help="Set the top level volume [required]",      metavar="VOLUME", default="CAVE" )
   parser.add_option( "--basepath",     dest="basepath", default="NONE" )
   parser.add_option( "--geompath",     dest="geompath", default="NONE" )

   parser.add_option( "--stat",         dest="stat",     default="radlen", help="Statistic to display" )

   
   parser.add_option( "--thumbnail",    dest="thumbnail", default=False, action="store_true",
                      help="Creates thumbnails of the front page of the PDF file." )
   parser.add_option( "--size", dest="size", default=False,
                      help="Sets the size of the thumbnail, e.g. 850x1100")
   
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

   # Setup temporary symbolic links to the root files
   if ( opts.basepath != "NONE" ):
      os.system("ln -s "+opts.basepath+"/"+opts.baseline+".root .")
   if ( opts.geompath != "NONE" ):
      os.system("ln -s "+opts.geompath+"/"+opts.geometry+".root .")      
   
   canvas = CanvasPDF(    name="differential-"+opts.baseline+"-vs-"+opts.geometry+"-"+opts.volume,
                          title="Geometry differential for volume="+opts.volume+ " "+opts.baseline+" vs "+opts.geometry,
                          nx=1,
                          ny=1,
                          thumbnail=opts.thumbnail
                          )


   differ = Differential( base=opts.baseline, 
                          comp=opts.geometry,
                          top=opts.volume,
                          basegeo=opts.basename,
                          compgeo=opts.geomname,
                          canvas=canvas,
                          stat=opts.stat
                          )

      

   # Remove temporary symbolic links to the root files
   if ( opts.basepath != "NONE" ):
      os.system("rm "+opts.basepath+"/"+opts.baseline+".root")
   if ( opts.geompath != "NONE" ):
      os.system("rm "+opts.geompath+"/"+opts.geometry+".root")      


if __name__ == '__main__':
   main()
