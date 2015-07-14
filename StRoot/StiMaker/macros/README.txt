README.txt

------------------------- Simple geant documenation ----------------------

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

------------------- MEVSIM Documentation --------------------

Mevsim is a simple event generator that makes a .nt file that can then be
run through geant.  Mevsim is part of the standar STAR library, and can thus be executed
without any setup by simply typing:

mevtupl.x

However, if you want to control the number, type, and kinematics of the generated
particles, you must make sure that you have the file mult_gen.in in the directory
from which you execute mevtupl.x.  The file mult_gen.in can be found in this
directory.  The controls are:

line 3:		number of events
line 14:	number of particle types

From there, you must provide a block of informatin for each particle type.  In the example in
this directory, you will see that we set line 14 at 7 and then we provide a block
of information for each of the seven particle types.  For an example, look at:

line 31:       Here we begin the block for geantId=8=pi+ (as you can see by comment)
line 32:       Mean number of pi+ / event, variance control

From there you can follow the comments to control the kinematics, etc.

Note: you must make sure that the number of particles you specify in 14 is
      equal to the number of information blocks that you specify.  That is, the
      code behavior is undefined if you specify 4 particle types and give 3 information
      blocks.

Then, to put the whole thing together you:

1) remove all mult_gen.log files in the current directory
2) make sure that you have a mult_gen.in file in the current directory
3) mevtupl.x

Now we should have generated a file evgen.X.nt where x is a number that gets
incremented each time you run mevsim in the same directory.  Next, we must
run these through geant.  This is accomplished by executing the staf macro
make_FZD.kumac.  You will find an example in this directory.  You must edit:

line 7:		 This must point to the evgen.X.nt file just created
line 8:		 This is the name of the output.fzd file

Then do:
1) staf
2) exec make_FZD.kumac

Then we can run these (finally) through bfc.C and do STAR reconstruction by:

root4star -b 'bfc.C(NEVENTS, "fzin evout mdc4 y2b", "OUTPUT.fzd")'

where NEVENTS is the number of events you want to reconstruct and
OUTPUT.fzd is the file generated from line 8 of make_FZD.kumac.

MLM
