In order to fit and project 3d histogramms using the dFitter3d and dProjector classes,
you can either compile the whole StHbtMaker and access the fitting routines as a part
of the shared lib StHbtMaker.so or you compile these classes standalone and run the fit 
seperated from the rest of the analysis.

To run it standalone, you only have to have ROOT running on your machine, no root4star, no afs, ...
you simply copy the classes dFitter3d and dProjector (.cxx and .h files) from StHbtMaker/Fit 
along with the Makefile and the LinkDef file from StHbtMaker/doc/dFitter
into a clean dir and compile everything using make. This will hopefully produce a shared lib 
called libfit.so.

This shared lib can be loaded into ROOT and the classes are usable the same way as they are
in root4star (note, you have to load the physics shared lib first: gSystem->Load("libPhysics.so").

Using the mfit.C makro as a template you should be able to use these classes for fitting.
