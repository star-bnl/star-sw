README.txt

The files protonstart.g and proton.kumac are files that can be used to generate and reconstruct n events with a fixed multiplicity, fixed charge, fixed particle type, and fixed momentum--truly monochromatic!  These files are poorly named, as they can be used for all particle species, not just protons.  Oh well...

The control-handles are:

protonstart.g:
--------------
line 42:	Particle type (6=muon, 14,15=proton, 11,12=kaon, 8,9=pion)
		differing in charge sign
line 50:	number of tracks
line 45:	momentum of particle (GeV)

proton.kumac:
-------------
line 18:	output file name (.fzd)
line 37:	number of events

Now, to put it all together do:
1) Sit in the directory with these files
2) staf
3) exec proton.kumac
4) After all of the staf craziness ends, quit staf
5) root4star -b 'bfc.C(NEVENTS, "fzin evout mdc4 y2b","OUTPUT.fzd")'
   where NEVENTS is an integer that is an upperbound on the number of events to
   reconstruct and OUTPUT.fzd is a char* that must be identical to that given in
   line18 of proton.kumac

The only other caviat is that the reconstruction code used will be tagged by the library version that you are using.  So, if you want to run ITTF code in, e.g., .DEV, you must generate these files from .DEV, as well.

In the near future, I will add information on how to use MEVSIM, which is a
simple event generator with much more flexibility.

MLM