Geant3 + VMC
=============

This package contains:

### Geant3.21

The new version of Geant3.21 that includes several bug fixes
compared to the standard version in CERNLIB.
In this version all Geant3 gxxxxx routines have been renamed g3xxxxx.
//run make in this directory. The Makefile will compile all Geant
//routines and a shared lib lib/tgt_Linux/libgeant321.so will be created.

### TGeant3 (Geant3 VMC)

The directory TGeant3 contains the classes TGeant3 and TGeant3TGeo,
which implement the  TVirtualMC interface, see more about VMC at: <br/>
[https://root.cern.ch/vmc](https://root.cern.ch/vmc)

### Examples

The directory examples includes a set of FORTRAN examples
like gexam1.F, gexam3.F, gexam4.F and model.F, plus the ROOT (C++) macros
E01.C, E02.C, E03.C

By default, TGeant3 is compiled with the option "WITHG3".
Specify -DWITHROOT in the compile options to select the ROOT geometry.
When the option WITHROOT is selected, TGeant3 will be generated
with calls to the Root geometry package only.
When the option WITHG3 is selected (default), TGeant3 will be generated
to use the Geant3 geometry package only.
Option WITHBOTH selects the two packages (only interesting to compare
the response from G3 compared to ROOT).

To build the examples gexam1,3 and 4 run the scripts bind_gexam1,3,4
bind_gexam1
```bash
  gexam1
  read 4
  stop
  root >  gMC->ProcessRun(10)
  root > .q
```
same for gexam3,4, model.

To run the E01.C, E02.C and E03.C examples (common to Geant3 and 4), do
root > .x E01.C

These scripts require the geant4_vmc file in a separate tar file.

At present, the geant3 package is tested only using the test suites defined
in geant4_vmc/examples.

