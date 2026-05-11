# RHICf Event Generators with CRMC2.2.1 (https://gitlab.iap.kit.edu/AirShowerPhysics/crmc)

# CRMC v2.2.0 Last modifications 2025/04/08

Please cite as:

> Ralf Ulrich, Tanguy Pierog, Colin Baus. "The Cosmic Ray Monte Carlo
> Package, CRMC (v2.2.0)". Zenodo
> (2025). https://doi.org/10.5281/zenodo.xxxxx


# The Program `crmc`

The program `crmc` (Cosmic Ray Monte Carlo) is an interface giving
access to different cosmic ray and non cosmic ray event generators by
an easy-to-use command line interface. The output can be stored in
different formats, i.e. in a ROOT `TTree`, HepMC event file, or Rivet
output directly. 

Version with extended arrays for heavy ion collisions.

# Authors

- C++ interface : Colin Baus
                Ralf Ulrich (ralf.ulrich@kit.edu)
- Fortran interface : Tanguy Pierog (tanguy.pierog@kit.edu)

`crmc` is loosely based on the first EPOS-ROOT interface by X. Garrido
(2009)

Special thanks to Christian Holm Christensen for his contributions to
migrate to HepCM3 as well as to directly write Rivet output.
And to Andrii Tykhonov for the interface to GEANT 4. Great!

# References

- CRMC : Reference : C. Baus, T. Pierog and R. Ulrich.
  See also: https://doi.org/10.5281/zenodo.4558705


## Hadronic interaction models :

### Post LHC :

- EPOS.LHC-R (`-m 0`) : T. Pierog and K. Werner [to be published]

- EPOS.LHC-R without hadronic rescattering (`-m 1`) : T. Pierog and K. Werner [to be published]

- QGSJETIII-01 (`-m 13`) : S.Ostapchenko, [Phys.Rev.D 109 (2024) 3,
                    034002](https://doi.org/10.1103/PhysRevD.109.034002)
                           S.Ostapchenko, [Phys.Rev.D 109 (2024) 9,
                    094019](https://doi.org/10.1103/PhysRevD.109.094019)

- QGSJETII-04 (`-m 7`) : S.Ostapchenko, [Phys.Rev. D83 (2011)
                    014018](https://doi.org/10.1103/PhysRevD.83.014018)

- SIBYLL2.3e (`-m 6`) : F. Riehn, R. Engel, A. Fedynitch,
  T.K. Gaisser, and T. Stanev, [Phys. Rev. D 102, 063002
  (2020)](https://doi.org/10.1103/PhysRevD.102.063002) and E.-J. Ahn,
  R. Engel, T.K. Gaisser, P. Lipari, and T. Stanev, [Phys.Rev. D80
  (2009) 09400](https://doi.org/10.1103/PhysRevD.80.094003)3

- DPMJETIII 2017-1 (`-m 12`) : Fedynitch at al.

- DPMJETIII 2019-1 (`-m 12`) : Fedynitch at al.

### Pre LHC :

- DPMJET 3.06 (`-m 12`) : [Phys. Rev. C 77, 014904
  (2008)](https://doi.org/10.1103/PhysRevC.77.014904)

- QGSJET01 (`-m 2`) : N.N. Kalmykov, S. Ostapchenko, and A.I. Pavlov,
  [Nucl.Phys. B (Proc. Suppl.) 52B (1997)
  17](https://doi.org/10.1016/j.nuclphysbps.2007.11.029)

- QGSJETII-03 (`-m 11`) : S. Ostapchenko, [Nucl.Phys.Proc.Suppl. 151
  (2006) 143](https://doi.org/10.1016/j.nuclphysbps.2005.07.026)

### Other models :

- PHOJET 1.12 (`-m 8`) : R. Engel, J. Ranft and S. Roesler,
  [Phys. Rev.  D52 (1995)
  1459](https://doi.org/10.1103/PhysRevD.52.1459) and F. W. Bopp,
  R. Engel and J. Ranft
  [arXiv:hep-ph/9803437](https://arxiv.org/abs/hep-ph/9803437).

- PYTHIA 6.4.28 (`-m 4`) : T. Sjostrand, S. Mrenna and P. Skands,
  [JHEP 0605 (2006) 026](https://doi.org/10.1088/1126-6708/2006/05/026

- HIJING 1.38 (`-m 5`) : obsolete (please use a more modern version
  like HIJING++)
  [Comput.Phys.Commun.83:307,1994](https://doi.org/10.1016/0010-4655(94)90057-4)

- GHEISHA (`-m 3`) : obsolete

# Installation

To install please get the latest version from the authors and install
the following pre-requisites: 

- [BOOST](https://www.boost.org/)
- [HepMC3](https://gitlab.cern.ch/hepmc/HepMC3)(*)
- [CMAKE](https://cmake.org)

Consider also the extensions provided by
[Rivet](https://rivet.hepforge.org/) (>v3) and
[fastjet](http://fastjet.fr/). We recommend to use a LTS Ubuntu
version to avoid compatibility problems (latest tests done using
24.04.02).

It is recommended to make a separate build directory next to the
source directory "crmc"

    mkdir build 
	cmake ../crmc

In order to compile with Fastjet, set the `Fastjet_DIR` environment
variable to the location of your Fastjet installation.

In order to compile with HepMC3, make sure that the `HepMC3-config`
tool is accessible via your PATH environment variable.

In order to compile with Rivet, make sure that the `rivet-config` tool
is accessible via your PATH environment variable.

Note, only one model is compiled by default: EPOS.LHC-R. You can
select many further models by passing "-DCRMC_SIBYLL" etc for all
available models to cmake. Check `CMakeLists.txt` for a list of
models, or even better use the `ccmake` or `cmake-gui` tool for all
configurations.

After you have done so, you can compile the program:

    mkdir -p installed
	cd installed
    cmake <DIRECTORY_WHERE_YOUR_SOURCE_IS> [any futher config like -DCRMC_QGSJETII04=ON]
    make install # you can compile faster with option --jobs=8

To test the installation you can use

    make test
or

    bin/crmc -T <your options>

if you want to test some specific set of options.


(*) HepMC2 is not recommended any more. The support is deprecated and
will disappear. Use HepMC3.

## CMake options 

Below is a short summary of some options of interest 

### Overall options 

- `CMAKE_INSTALL_PREFIX` (`PATH`, default build directory) Where to
  install CRMC (application, libraries, headers, tables).  If
  `CMAKE_INSTALL_PREFIX` is set to `<prefix>`, then the installation
  is as follows 
  
      <prefix> -+- bin           Application crmc 
	            +- etc           Parameter file crmc.param
				+- include/crmc  Header files 
				+- lib           Libraries 
				+- share/crmc    Data tables 

- `CMAKE_BUILD_TYPE` (`STRING`) Type of build - f.ex. `RelWithDebInfo`
  for optimised built including debug symbols. 
  
- `CRMC_EXT_RNDM` (`BOOL`, default `OFF`)
  Enable external (system) random number generator.

- `CRMC_PROG` (`BOOL`, default `ON`) Enable building the `crmc`
  application.  If this is set to `FALSE`, then libraries of CRMC will
  still be built (static or shared) and installed together with
  relevant header files.

- `CRMC_STATIC` (`BOOL`, default `OFF`) If set to `ON`, then the
  application `crmc` will be statically linked to all enabled models
  and interfaces.  That means that the application is relatively
  stand-alone.  If set to `OFF`, then shared libraries, including the
  basic code will be built.  Models will be loaded at run-time via
  these libraries. 
  
  Note, if this option is set to `ON`, then 3rd-party applications may
  use the library `libCrmc.so` (and headers in
  `CMAKE_INSTALL_PREFIX/include/crmc`) to integrate CRMC. 
  
### Interface options 

- `HepMC3_DIR` (`PATH`) Where to find HepMC3 installation 

- `HepMC_HEPEVT_SIZE` (`STRING`, default 10000) size of the `HEPEVT`
  common block. This sets the maximum number of particles that can be
  output by the application.  When simulating heavy-ion collisions at
  high energies, it is _imperative_ to enlarge this number (we
  recommend 200000). 

- `HepMC_PATH` (`FILEPATH`) Where to find the HepMC(2) installation
  (deprecated - use HepMC3 instead - but still installed by default and can be combined with HepMC3). 

- `fastjet_PATH` (`FILEPATH`) Where to find the FastJet installation. 

- `RIVET_DIR` (`PATH`) Where to find the Rivet installation. 

- `CRMC_ENABLE_ROOT` (`BOOL`, default `OFF`)
  Enable ROOT output.  Requires ROOT to be found. Deactivate HepMC2.

- `CRMC_ENABLE_HepMC3` (`BOOL`, default `ON`)
  Enable HepMC3 output.  Requires HepMC3 to be found. 

- `CRMC_ENABLE_Rivet` (`BOOL`, default `OFF`)
  Enable Rivet output.  Requires Rivet to be found. Deactivate HepMC2.

- `CRMC_GEANT4` (`BOOL`, default `OFF`)
  Enable GEANT 4 interface. 

- `CRMC_GEANT4PHYS` (`BOOL`, default `OFF`)
  Enable CRMC as a GEANT 4 physics list interface. 


### Model options
- 
- `CRMC_DPMJET06` (`BOOL`, default `OFF`) Enable DPMJet version 3.06.
  Note, only one DPMJet option can be enabled.

- `CRMC_DPMJET17` (`BOOL`, default `OFF`) Enable DPMJet version 2017.
  Note, only one DPMJet option can be enabled.

- `CRMC_DPMJET19` (`BOOL`, default `OFF`) Enable DPMJet version 2019.
  Note, only one DPMJet option can be enabled.

- `CRMC_GHEISHA` (`BOOL`, default `OFF`) Enable the (obsolete) GHEISHA
  model

- `CRMC_HIJING` (`BOOL`, default `OFF`) Enable the HIJING model (Note,
  HIJING 1 is now obsolete, but HIJING 3 has still not been released
  to the public).

- `CRMC_PHOJET` (`BOOL`, default `OFF`) Enable the PhoJet model.

- `CRMC_PYTHIA` (`BOOL`, default `OFF`) Enable the Pythia 6 model.
  Note, Pythia 6 is now obsolete.  Consider using Pythia 8 instead. 

- `CRMC_QGSJET01` (`BOOL`, default `OFF`) Enable the QGSJET01 model.

- `CRMC_QGSJETII03` (`BOOL`, default `OFF`) Enable the QGSJET-II-03
  model.

- `CRMC_QGSJETII04` (`BOOL`, default `OFF`) Enable the QGSJET-II-04
  model.

- `CRMC_QGSJETIII` (`BOOL`, default `OFF`) Enable the QGSJET-III-01
  model.

- `CRMC_SIBYLL` (`BOOL`, default `OFF`) Enable the Sibyll model.

# Troubleshoot

Please first try in your build diretory to run:

	make clean
	rm CMakeCache.txt

Sometime it can happen that the CMake cache still contains
old options or set paths that should be updated. In this
case the issues can be resolved by doing that.

On MacOS, please switch to STATIC libraries in CMakeLists.txt (option
`CRMC_STATIC`) 

# Run

Run the program by executing

	./bin/crmc -h

to get the following help:
    USAGE: 
    
       ./crmc  [-L <string>] ... [-r <string>] ... [-a <string>] ... [-x] [-T]
               [-t] [-f <string>] [-c <string>] [-S <double>] [-I <int>] [-P
               <double>] [-i <int>] [-p <double>] [-m <int>] [-n <int>] [-s
               <int>] [-o <string>] [--] [--version] [-h]
    
    
    Where: 
    
       -L <string>,  --preload <string>  (accepted multiple times)
         add preloaded data to Rivet
    
       -r <string>,  --rivet-include <string>  (accepted multiple times)
         add to Rivet search path
    
       -a <string>,  --analysis <string>  (accepted multiple times)
         Rivet analysis to execute
    
       -x,  --cross-section
         calculate and print cross section only
    
       -T,  --test
         Run test mode
    
       -t,  --produce-tables
         create tables if none are found
    
       -f <string>,  --out <string>
         output file name (auto if none provided)
    
       -c <string>,  --config <string>
         config file
    
       -S <double>,  --sqrts <double>
         sqrt(s/GeV**2)
    
       -I <int>,  --target-id <int>
         PDG or Z*10000+A*10 (default: proton)
    
       -P <double>,  --target-momentum <double>
         momentum/(GeV/c) (default: -3500)
    
       -i <int>,  --projectile-id <int>
         PDG or Z*10000+A*10 (default: proton)
    
       -p <double>,  --projectile-momentum <double>
         momentum/(GeV/c) (default: 3500)
    
       -m <int>,  --model <int>
         model [0=EPOS.LHC-R (default), 1=EPOS.no.had.resc, 2=QGSJET01, 3=Gheisha,
         4=Pythia_6.4.28, 5=Hijing_1.38, 6=Sibyll_2.3e, 7=QGSJETII-04, 8=Phojet
         , 11=QGSJETII-03, 12=DPMJet-III_2019.1, 13=QGSJETIII_01]
    
       -n <int>,  --number <int>
         number of collisions (default: 500)
    
       -s <int>,  --seed <int>
         random seed between 0 and 1e9 (default: random)
    
       -o <string>,  --output_mode <string>
         hepmc, hepmcgz, hepmc3, hepmc3gz (default), root, lhe, lhegz, rivet
    
       --,  --ignore_rest
         Ignores the rest of the labeled arguments following this flag.
    
       --version
         Displays version information and exits.
    
       -h,  --help
         Displays usage information and exits.

For projectile and target Id the following shortcuts are allowed:

| Number | PDG        | Description        |
|--------|------------|--------------------|
|  1     |  2212      | proton             |
| -1     | -2212      | anti-proton		   |
|  12    | 1000060120 | Carbon			   |
|  120   |  211       | pion+ (not for -I) |
| -120   | -211       | pion- (not for -I) |
|  130   |  321       | kaon+ (not for -I) |
| -130   | -321       | kaon- (not for -I) |
|  208   | 1000822080 | Lead               |

using these shortcuts with automatic output file name generation will
create more human readable names.

The environment variable `$CRMC_OUT` can be set to define the path the
path of the output files, otherwise `$PWD` is used as default path.

**Example** to generate 100 7 TeV pp collisions with EPOS.LHC-R:

    bin/crmc -o hepmc -S 7000 -n 100 -m 0

**Example** to generate 100 1.38 ATeV PbPb collisions with EPOS.LHC-R but not using hadronic rescattering :

    bin/crmc -o hepmc -p 1380 -P -1380 -n 100 -i 208 -I 208 -m 1

**Example** to generate 100 5.02 ATeV pPb collisions with QGSJetII-04:

    bin/crmc -o hepmc -p 4000 -P -1577 -n 100 -m 7 -i 2212 -I 822080

**Example** to test Sibyll2.3e

    bin/crmc -T -m 6

## On Rivet output

If you select Rivet as output CRMC (option `o rivet`) will produce a
yoda file with analysis output for you. Of course, you have to specify
suited Rivet analyses using the `-a` option (e.g. `-a
CMS_2011_S9120041`). You may combine as many analyses as you wish in
multiple `-a` options. You can also define specific Rivet search path
and preloads using the `-r` and `-L` options, respectively. 

# Options

The details of the run can be controlled by the file `crmc.param`.

In this file the 4 first commented options are valid for EPOS.LHC-R only.

- By defaults EPOS models use a simplified treatment of QGP in events
  where the energy density is high enough (including in pp). If you
  uncomment the line 
  
	  !core off
	  
  this will be disable and running time will be shorter (but data
  description will be worth since the physics model will be
  incomplete).

	  !switch fusion off
	  
  the core-corona will be disable AND the parameters are tuned not taking
  into account collective effects (proper multiplicity and different particle
  yield). Running time will be shorter. In that mode EPOS is comparable to
  PYTHIA model without color reconnection (no final state interaction).

  Remark : both modes are compatible with m=0 and m=1 but the parameters for
  "switch fusion off" are tuned in the case m=1 (without hadronic rescattering)
  (the default parameters are tuned for m=0 (with hadronic rescattering) AND "core on")

- By default only the final particles are recorded in the output file
  like for other cosmic ray models.  Uncommenting

	  !set istmax 1
	  
  allows the user to have the full chain of mother/daughter from the
  beam to the final particles with EPOS models.
  
  The outfile is at least 2 times larger but includes the decayed
  particles and some special intermediate particles between the beam
  and the real particles which allow the user to know where the
  particles were generated. The ids of such particles are the
  following:

  - 90 : sum of all spectators in case of nuclear beam. It is the
    mother particle of final nuclear fragments (nucleons and light
    nuclei). 
  
  - 91 : cluster which will produce particle statistically. Mother of
	particles coming from mini-(quark-gluon) plasma in EPOS. 
  
  - 92 : string. Mother of particles coming directly from string
	fragmentation of initial Pomerons (which do not participate to
	plasma formation). 
  
  - 93 : remnant. Mother of particles coming from the beam remnants.

  Each primary particle has a mother with id 9x. For technical reasons
  all particles with the same id and same momentum are in fact the
  same initial object (cluster, string or remnant) which was split in
  different local pieces to keep the correct production vertexes of
  the final particles. If a set of particles with the same id (91 or
  92) and the same momentum has 2 different mothers (one from the
  projectile and one from the target) it means that the string was
  produced by the interaction of this pair of nucleons or for a
  cluster that it was the closest pair of nucleon which participate at
  the formation of the cluster (a cluster is formed by string pieces
  of different pairs but hepmc format allows only one mother in that
  case).

- By default the cross-section is calculated by a numerical method
  with is valid only for _h_-p (_h_ being pion, kaon or
  nucleon) but not AB (nucleus-nucleus). The inelastic cross-section
  is tabulated but the total or elastic ones. If you uncomment 
  
	  !set isigma 2
	  
  all the nuclear cross-sections will be calculated but it takes
  several minutes to compute it (see Note on simulating Heavy Ion
  events).

## Note on Simulating Heavy Ion (HI) Events

QGSJET01 and SIBYLL2.3e are limited to h-B or AB collisions with A<64 (B<19 for SIBYLL)
and h being a pion, a kaon or a nucleon.

Only EPOS and QGSJET (II-03, II-04 and III) models can run (h)AB
collisions with A and B up to 208.

QGSJETII or QGSJETIII were never designed for HI collisions, so results should be
interpreted with care (no final state interactions).

EPOS.LHC-R includes a simplified treatment of QGP hadronization
but doesn't include full hydro simulations as already
possible in EPOS 4(*). The default version (m=0) is slow because hadronic reinteraction
are taken into account using URQMD3.4 even in light systems. It gives a good overall
description of HI data but it should not be used to interpret QGP related observables
(correlation, flow, jet quenching, etc ...) since the model is oversimplified for this
distributions.
Note that due to the hadronic rescattering not all the (short live) resonances (like rhos, K*, phi, eta', ...)
will not be found in mothers of the final list of particle (if istmax is set to 1). They can
be found like in real data by invariant mass reconstruction of the decay products. Some
may appear in the list, but even for a given particle type, it will be only a part of them.
Using m=1, all the mothers of decayed particles will be visible with istmax=1.

Without hadronic rescattering (m=1) it is much faster but in particular,
the baryon (and in particular nucleons) distributions won't describe the data properly.

(*)For detailed simulations of HI collisions within EPOS 4, please
contact K. Werner (werner@subatech.in2p3.fr)
or check https://klaus.pages.in2p3.fr/epos4/ .*


## Only for old HepMC2

The HepMC (2.06) IO library has a default limitation of the number of
produced particles of 10000.  Thus, events with more than that number
of secondaries are either truncated or skipped. For heavy ion
collisions this easily can become a problem in CRMC. Please consider
changing this limitation to a higher value. We suggest 200000, which
works for all models at LHC energies so far. We are happy for feedback
on this issue if problems are encountered.  

In order to change the limit, define the CMake variable 

    HepMC_HEPEVT_SIZE 
	
to a suitable number - e.g., 

    cmake -DHepMC_HEPEVT_SIZE=200000 ...

## For old HepMC2 and ROOT

HepMC2 and ROOT output gives access to the signal_process_id which follow the ISUB (MSUB)
definition of MB processes in PYTHIA except for 96, 97 and 98:
 91: elastic (AB->AB) (no produced by all models)
 92: single diffraction SD (proj excit. AB->X-gap-B)
 93: single diffraction SD (targ excit. AB->A-gap-X)
 94: double diffraction DD (AB->X-gap-X)
 95: non-diffractive ND (AB->X)
 96: non-diffractivs ND with core formation
 97: central diffraction CD (AB->A-gap-X-gap-B)
 98: pion exchange (all processes with pion or rho exchange)


# Example For Analysing HepMC3/ROOT Output Files

In the directory [`./ExampleAnalyser`](ExampleAnalyser) you can find
code for a small [example](ExampleAnalyser/src/analysis.cc) that loops
over the HepMC3 and HepMC3gz files and its particles.  To build the
example make sure you have boost, and hepmc3 
setup as for the normal installation of crmc then run

    make 
    
and 

    bin/analyse
    
to analyse your data files. 
	
There is also a corresponding [example
analysis](ExampleAnalyser/analysisROOT.cc) reading ROOT output files.

We also provide a `CRMC.ipynb` notebook to read and display HepMC3
data with python directly.

