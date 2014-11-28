#!/usr/bin/env python
import os
import sys

from optparse import OptionParser

"""

User interface to the Star Baseline Analysis for the Simulation Environment (StarBASE).

starbase.py is a python script used to run the StarBASE application.  The script and
application performs the following actions:

1) Translates the requested AgSTAR geometry tag (e.g. y2006h) into the root format.
2) Create histograms for every volume in the geometry
3) Throw geantinos from the origin (x,y,z)=0 over the user-specified fiducial volume
    (default is -6 < eta < 6 and -15 < phi < +15 degrees)
4) Fill each histogram with the amount of material (in number of radiation lengths)
   which is contained by that volume (inclusive of all daughter volumes).

Upon program completion, a root file is generated with the name (geom).root, e.g.
y2006h.root.  The histogram contains several 1D histogram vs. pseudorapidity (eta).

      h_radlen_VOLUME_eta -- Each bin is the sum over all tracks passing betwen eta
                             and eta+eta of the number of radiation lengths of material
                             encountered (n = L/chi_0).
      h_counts_VOLUME_eta -- Each bin contains the total number of tracks passing
                             between eta and eta+deta.

Python (PyROOT) scripts are available under StarVMC/StarBASE/macros/ to process the
resulting histogram files.  The two files which are needed are

1) starbase.py
2) differential.py

Example usage:

$ starbase.py --geom=y2006 
$ starbase.py --geom=y2006h
$ differential.py --base=y2006 --geom=y2006h

--------------------------
"""

# ----------------------------------------------------------------------------
def libs():

    from ROOT import gSystem
    
    gSystem.Load("libVMC.so")
    gSystem.Load("St_base.so")
    gSystem.Load("St_Tables.so")
    gSystem.Load("StUtilities.so")
    gSystem.Load("StChain.so")
    gSystem.Load("StarVMCApplication.so")
    gSystem.Load("libStarMiniCern.so")
    gSystem.Load("libgeant3.so")
    gSystem.Load("StarBASE.so")

# ----------------------------------------------------------------------------
def main():

    #
    # Add path to macros to module search path
    #
    sys.path.append(os.environ['ROOTSYS'] + "/lib")    
    sys.path.append("StarVMC/StarBASE/macros")


    #
    # Parse command line options
    #
    parser = OptionParser()
    parser.add_option( "--geom",    help="Sets the geometry to be processed",     dest="geom",   default="NONE" )
    parser.add_option( "--name",    help="Sets the name of the geometry [default is tag name]", dest="name", default="NONE" )
    parser.add_option( "--etamin",  help="Minimum pseudorapidity cut",            dest="etamin", default=-6.0,    type="float" )
    parser.add_option( "--etamax",  help="Maximum pseudorapidity cut",            dest="etamax", default=+6.0,    type="float" )
    parser.add_option( "--phimin",  help="Minimum azimuthal angle cut [degrees]", dest="phimin", default=-15.0,   type="float" )
    parser.add_option( "--phimax",  help="Maximum azimuthal angle cut [degrees]", dest="phimax", default=+15.0,   type="float" )
    parser.add_option( "--top",     help="Top volume.  All volumes below this volume will be probed", dest="top", default="HALL" )
    parser.add_option( "--wrapper", help="Set false for native execution (don't do this)",  dest="wrapper",   default=True )
    parser.add_option( "--verbose", help="Default 1.  0 supresses all output.  >1 shows output of root, zebra, etc...", dest="verbose", default=0 )
    parser.add_option( "--path",    help="Define the output path",                dest="path",   default="./" )

    parser.add_option( "--makegeom",  help="Default=false.  If true, builds geometry from agstar.", dest="mkgeom",default=False)
    parser.add_option( "--lib",       help="Sets the library to build the geom from.  Default $STAR_LIB", dest="library",default="$STAR_LIB")
    (opts, args) = parser.parse_args()

    if ( opts.geom == "NONE" ):
        print ""
        print "Must specify the geometry with --geom=TAG."
        print ""
        os.system("./starbase.py --help")
        return 0

    from ROOT import TROOT, gROOT

    #
    # Run starsim and translate requested geometry
    # into the ROOT format
    #
    if opts.mkgeom:
        make_geometry( opts, opts.library )
    

    if ( opts.wrapper ):

        name = opts.geom
        if ( opts.name != "NONE" ):
            name = opts.name

        if ( opts.verbose > 0 ):
            print "++ Generating MC for "+opts.geom+" in volume "+opts.top
            
        cmd = 'root.exe -l -q -b StarVMC/StarBASE/macros/starbase.C\\(\\\"'+name+'\\\",'+str(opts.etamin)+','+str(opts.etamax)+','+str(opts.phimin)+','+str(opts.phimax)+',\\\"'+opts.path+'\\\"\\)'
        if ( opts.verbose > 1 ):
            os.system( cmd )
        else:
            os.system( cmd + '>& /dev/null' )


    else:

        #
        # Source the STAR rootlogon script
        #
        gROOT.Macro("rootlogon.C")
        
        #
        # Load shared libraries needed to run the code
        #
        libs()
                       
        #
        # Run the chain
        #
        chain = make_chain( options=opts )

# ----------------------------------------------------------------------------
def make_geometry( opts, library="$STAR_LIB" ):

    geometry = opts.geom
    name     = geometry
    if ( opts.name != "NONE" ):
        name = opts.name

    path = opts.path

    if ( opts.verbose > 0 ):
        print "++ Converting geometry "+geometry+" to ROOT geometry format"

    file = open("/tmp/"+geometry+".kumac","w")
    file.write("macro "+geometry+"\n")
    file.write("detp geom "+geometry+"\n")
    file.write("gexec "+library+"/geometry.so\n")
    file.write("gclos all\n")
    file.write("grfile /tmp/"+geometry+".rz\n")
    file.write("exit\n")
    file.write("return")
    file.close()
    
    for line in os.popen( 'starsim -w 0 -b /tmp/'+geometry+'.kumac' ).readlines():
        pass # execute silently

    for line in os.popen( 'g2root /tmp/'+geometry+'.rz '+name+'.h' ).readlines():
        pass # ditto

    os.system('mv '+name+'.h '+path+'/'+name+'.h')

    #
    # Make the geometry available to root
    #
    if ( opts.wrapper == False ):
        gROOT.LoadMacro( name + ".h" )
        gROOT.ProcessLine( name + "()" )
    


# ----------------------------------------------------------------------------
def make_chain( options ):

    from ROOT import StChain
    from ROOT import StarBASE
    from ROOT import StMCStepping
    from ROOT import gGeoManager    
    from ROOT import TFile

    print "++ Running VMC/StarBASE with geometry "+options.geom
    
    # Create the chain
    chain = StChain("chain")

    # Create the baseline analysis maker
    base  = StarBASE("base", options.geom, 1)

    # Setup kinematic limits and divisions
    base.SetAttr("eta_min", options.etamin )
    base.SetAttr("eta_max", options.etamax )
    base.SetAttr("deta",    0.02)
    base.SetAttr("phi_min", options.phimin )
    base.SetAttr("phi_max", options.phimax )
    base.SetAttr("dphi",    0.120)

    # Initialize the chain
    chain.Init()

    # Book histograms
    base.steps().bookVolume( options.top )

    #   chain.EventLoop(1)
    chain.Make()
    chain.Finish()

    # Open a new TFile and output the histograms
    file = TFile( options.geom+".root", "recreate" )
    base.GetHistList().Write()
    gGeoManager.Write()
    
    file.Close()

    return chain
    
if __name__ == '__main__':
    main()

