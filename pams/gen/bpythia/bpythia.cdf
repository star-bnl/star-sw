*
* pythia 6.1 steering data cards
*
>Name bpythia
>Menu PYTHIA

>Guidance
The PYTHIA menu allows to read the old-fashion FFREAD
parameter input for the new (double precision) PYTHIA 
versions 6.1 and 6.2.

>Command  FRAME
>Guidance 
Select the collision frame - Center Of Mass, Fix Target, etc
In the case of CMS frame, one can set the CMS energy using
the ENER command.  Otherwise, set beam and target momenta
using pblue and pyell.
>Parameters
frame   'collision frame'                    C D=CMS
>action apytuser

>Command  BEAM
>Guidance 
Describes the composition of the collision - PP, ee, etc. 
Character variables to specify beam and target particles. 
Upper-case and lower-case letters may be freely mixed
>Parameters
beam   'composition of the incident beam'    C D=P
target 'composition of the target (beam)'    C D=P
>action apytuser

>Command PBLUE
>Guidance
Provides the blue beam (+z) three-momentum.
>Parameters
px 'x-component in GeV' R D=0.0
py 'y-component in GeV' R D=0.0
pz 'z-component in GeV' R D=250.0
>action apytuser

>Command PYELL
>Guidance
Provides the yellow beam (-z) three-momentum.
>Parameters
px 'x-component in GeV' R D=0.0
py 'y-component in GeV' R D=0.0
pz 'z-component in GeV' R D=-250.0
>action apytuser


>Command  ENER
>Parameters
energy  'collision energy'                   R D=14000
>Guidance 
Select the collision energy in the current frame.
Default is LHC nominal energy - 7 + 7 Tev.
>action apytuser

>Command  MSEL
>Parameters
msel  'hard scattering subprocess selector' I D=1
>Guidance 
Selection of hard scattering subprocesses
>action apytuser

>Command  TUNE
>Parameters
tune  'index of the pythia tune' I D=100 
>Guidance
Selection of the pythia tune.  Default corresponds to CDF tune A.
Consult the pythia manual (or comments in source code of pytune.F) 
for the available options. 

For pythia 6.3 and earlier:
  http://home.thep.lu.se/~torbjorn/Pythia.html

For pythia 6.4:
  http://projects.hepforge.org/pythia6/

Example:
* Sets CDF tune A 
  TUNE 100 
>actions apytuser 

>Command  PMAS
>Parameters
pmas   'given particle code and selector set mass value' C
>Guidance 
provide a mass value for a particle defined with its index
>action apytuser

>Command  PMA1
>Parameters
index_mass   'index and mass value' C
>Guidance 
provide a mass value for a particle defined with its index
>action apytuser

>Command  PMA2
>Parameters
index_mass   'index and mass value' C
>Guidance 
provide a mass value for a particle defined with its index
>action apytuser

>Command  PMA3
>Parameters
index_mass   'index and mass value' C
>Guidance 
provide a mass value for a particle defined with its index
>action apytuser

>Command  PMA4
>Parameters
index_mass   'index and mass value' C
>Guidance 
provide a mass value for a particle defined with its index
>action apytuser

>Command  MDME1
>Parameters
decay_modes  'index and decay mode value' C
>Guidance 
provide a decay mode value for a particle defined with its index
>action apytuser

>Command  CKIN
>Parameters
ckin   'hard scattering parameters 1-200' C
>Guidance 
Hard scattering kinematics cuts like Emax, Pt.
Most important of these are CKIN(1)-CKIN(8), CKIN(27)-CKIN(28)
>action apytuser

>Command  MRPY
>Parameters
mpry   'key 1-6' C
>Guidance 
Random number setting for Pythia internal random
number generator. If not set, the built-in starsim
generator will be used instead.
>action apytuser

>Command  MSUB
>Parameters
msub   'Process key 1-500' C
>Guidance 
Switch on a selected process.
>action apytuser

>Command  MSTP
>Parameters
mstp   'Pythia parameters 1-200' C
>Guidance 
Pythia parameters:
 MSTP(1) - maximum number of generations.
 MSTP(2) - calculation of alpha-s at hard interaction.
 MSTP(3) - selection of Lambda value in alpha-s.
 MSTP(4) - treatment of the Higgs sector.
 MSTP(5) - presence of anomalous couplings in processes.
 MSTP(7) - choice of heavy flavour in subprocesses.
>action apytuser

>Command  PARP
>Parameters
msub   'hard scattering paramters 1-200' C
>Guidance 
some of the parameters are :
 PARP(1) - used in running Alpha_s for hard scattering
 PARP(2) - lowest energy for the event as a whole that 
the program will accept to simulate
 PARP(13) - maximum scale allowed for photoproduction when using the option
 PARP(14) - in the numerical integration of quark and gluon parton 
distributions inside an electron, the successive halvings of evaluation 
point spacing is interrupted when two values agree in relative size.
>action apytuser

>Command  MSTU
>Parameters
mstu   'just paramters 1-200' C
>Guidance 
 MSTU(1)-MSTU(3) Variables used by the event study routines
 MSTU(4) number of lines available in the common block PYJETS
 MSTU(5) is used in building up the special colour-flow information
>action apytuser

>Command  PARU
>Parameters
msub   'just paramters 1-200' C
>Guidance 
 PARU(1)  - pi approx 3.141592653589793
 PARU(2)  - 2pi appro 6.283185307179586
 PARU(3)  - conversion factor for 1/GeV to fm
 PARU(4)  - conversion factor for fm 1/GeV
 PARU(5)  - conversion factor for 1/GeV^2 to mb
 PARU(6)  - conversion factor for mb to 1/GeV^2
 PARU(11) - relative error 
 PARU(12) - effective cut-off in squared mass
 PARU(13) - effective angular cut-off in radians for recombination of partons
>action apytuser

>Command  MSTJ
>Parameters
msub   'just paramters 1-200' C
>Guidance 
some useful parameters are :
 MSTJ(1) - choice of fragmentation scheme
 MSTJ(2) - gluon jet fragmentation scheme in independent fragmentation
 MSTJ(3) - energy, momentum and flavour conservation options in independent fragmentation
 MSTJ(11) - choice of longitudinal fragmentation function
 MSTJ(12) - choice of baryon production model 
 MSTJ(13) - generation of transverse momentum for endpoint quark(s)
 MSTJ(14) - treatment of a colour-singlet parton system with a low invariant mass
>action apytuser

>Command  IMSS
>Parameters
msub   'SUSY Treatment 1-200' C
>Guidance 
SUSY treatment key (page 199) :
 IMSS(1) level of MSSM simulation
 IMSS(2) treatment of U,SU and SU(3) gaugino mass parameters
 IMSS(3) treatment of the gluino mass parameter
 IMSS(4) treatment of the Higgs sector
 IMSS(5) allows you to set the st sbo and stau masses and mixing by hand
>action apytuser

>Command  RMSS
>Parameters
msub   'SUSY Parameters 1-200' C
>Guidance 
SUSY Parameters (page 200):
 RMSS(1) - is the common gaugino mass
 RMSS(4) - fixes the sign of the higgsino mass
 RMSS(5) - the ratio of Higgs expectation
 RMSS(2) - gaugino mass
 RMSS(6) - Left slepton mass 
 RMSS(7) - Right slepton mass
 RMSS(8) - Left squark mass
 RMSS(9) - Right squark mass 
>action apytuser


>Command  PARJ
>Parameters
msub   'suppresion paramters 1-200' C
>Guidance 
some useful parameters are:
 PARJ(1) the suppression of diquark-antidiquark pair
production in the colour field.
 PARJ(2) the suppression of s quark pair production in
the field compared with u or d pair production.
 PARJ(3)  - PARJ(10) the suppression of quarks
 PARJ(11) - PARJ(17) spin of mesons
>action apytuser


>Command  MDCY
>Parameters
mdcy  'compressed particle code (KC), index and decay value' C

>Guidance 
MDCY(KC,1) switch on/off the decay of particle defined by its 
compressed code

MDCY(KC,2) gives the entry point into the decay channel table 
for compressed particle code KC

MDCY(KC,3) gives the total number of decay channels defined 
for compressed particle code KC

 Example:  MDCY (25,1) = 5
 or:       MDCY1 25=5
 (courtesy of Mercedes Paniccia).
>action apytuser


>Command  MDCY1
>Parameters
mdcy1  'particle code and decay on/off switch' C
>Guidance 
MDCY(KC,1) switch on/off the decay of particle defined 
by its compressed code, example MDCY1 25 = 5. 
>action apytuser

>Command  MDCY2
>Parameters
mdcy2  'particle code and entry point into the decay channel table' C
>Guidance 
MDCY(KC,2) gives the entry point into the decay channel
table for compressed particle code KC
>action apytuser

>Command  MDCY3
>Parameters
mdcy3  'particle code and total number of decay channels for the particle' C
>Guidance 
MDCY(KC,3) gives the total number of decay channels defined for 
compressed particle code KC
>action apytuser


>Command KFPR
>Parameters
kfpr 'set flavours for products of a given subprocess' C
>Guidance 
given subprocess code (ISUB:1-200) and product index (J:1-2) 
set product flavour (KF)

 example:  KFPR (121,1) = 25.
 or        KFPR1 121 = 25.
 (courtesy of Mercedes Paniccia).

>action apytuser

>Command KFPR1
>Parameters
kfpr1  'first product subprocess code and product flavour' C

>Guidance 
set flavours(KF) for products of a given subprocess(1-200), 
example:  KFPR1 121 = 25.

>action apytuser

>Command KFPR2
>Parameters
kfpr2  'second product subprocess code and product flavour' C

>Guidance 
set flavours(KF) for products of a given subprocess(1-200), 
example:  KFPR2 121 = 5
(courtesy of Mercedes Paniccia).

>action apytuser

*>Command  ptab
*>Command  isub
*>Command  forc
*>Command  stab
*>Command  dmod

*End of Pythia Menu

