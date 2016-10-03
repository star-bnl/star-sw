Tauola C++ Interface
--------------------
Web page  http://www.ph.unimelb.edu.au/~ndavidson/tauola/doxygen/

Documentation in documentation/latex_documentation, type "make" there

To compile the interface:
- Execute "./configure" (if path to HepMC is not set with
                        HEPMCLOCATION, use "./configure --with-hepmc=<path>"
                        Use "./configure --without-hepmc" to compile
                        without HepMC interface)
- Execute "make"

--------------------------------------
  Alternatively cofiguration scripts based on autotools can be used.
  In this case 'cd platform', './use-LCG-config.sh' and 
  follow the instructions in the freshly created './INSTALL' readme file.
  The configure file in the directory 'examples' will be absent.
  The instructions collected below should be then ignored.
  Under MAC OS one may need to remove '-lfreetype' from
  'examples/Makefile'
--------------------------------------

To run the examples, "cd examples". There "./configure" and "make" as well.
For some of the examples to work, pythia8.1 and
MC-Tester will be needed. The most basic example does not need  them.

NOTE:
On CERN platforms (lxplus, lxplus4, lxplus5...) software located 
in the /afs/cern.ch/sw/lcg/ directory can be used. for that purpose execute:

source platform/afs.paths.sh

prior to  "./configure" being executed without any parameters. Same paths will
be used by examples there;  "./configure" in the examples direcotry after
compiling the interface as well. 

Paths defined in platform/afs.paths.sh:

HEPMCLOCATION=/afs/cern.ch/sw/lcg/external/HepMC/2.03.09/slc4_amd64_gcc34
PYTHIALOCATION=/afs/cern.ch/sw/lcg/external/MCGenerators/pythia8/135/slc4_amd64_gcc34
MCTESTERLOCATION=/afs/cern.ch/sw/lcg/external/MCGenerators/mctester/1.23.1/slc4_amd64_gcc34
ROOTSYS=/afs/cern.ch/sw/lcg/app/releases/ROOT/5.22.00/slc4_amd64_gcc34/root

can be modified.



Last Modified: T. Przedzinski, 28 Nov. 2011
