
SOURCES = hwigpr.f herwig6507.f heptup.f 

# Include files (for printout, below)
INCFILES = 

PROGRAM = herwig_tupl.x

#	Names of source files and derived names of .o files
#	Use pattern matching rules to make
OBJECTS = $(SOURCES:.f=.o)

#       Library assignments: if you want to link the CERN library, replace the
#       assignment with the correct path on your local host.
LIBS =  
LIBS =   -L${CERN_ROOT}/lib  -lmathlib -lgraflib -lgrafX11 -lpacklib_noshift -lkernlib_noshift
#	Lintfiles can derive the old fashioned way
#	Don't need them for nr's, yet.
LINTFILES = $(SOURCES:.f=.ln)

#
#	.KEEP_STATE: enables the "memory" of the make program
#
#.KEEP_STATE:
# 
# Flags for default macros to compile and link: if you want optimized
# executable, use "-O3", if you want a debug version, use "-g ..."
#CC = pgf77 
#CC = g77
CC = gfortran 
#CFLAGS = -O 
CFLAGS = -g -w -O -fno-second-underscore -fno-automatic   
CPPFLAGS =
LDFLAGS = 
LINTFLAGS = 

#	List of variants one can make.  all is the default.
#
all: $(PROGRAM)

#	Conditional Macro assignments for debug and profile
#
# debug := CFLAGS = -g		# debug flag (cannot optimize)
# profile := CFLAGS = -pg -O	# profile/debug flag, optimized code

#	necessary so that we can keep $(OBJECTS) in a different directory
#	than $(SOURCES), which we must be able to do with numerical
#	recipes sources.  $(LIBS) must be LAST, and in that -lm must
#	be last of all!

$(PROGRAM): $(OBJECTS) $(INCFILES)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIBS) 

#	We give rule for objects here

.f.o:
	$(CC) -c $(CFLAGS) $<


#	If we want a lintfile update, we "make lint"
lint:	$(LINTFILES)
#	with this rule: (to avoid making the lintfiles themselves)
$(LINTFILES):
	lint $(SOURCES)
#	NOTE:  If lint files are desired, comment out the previous
#	two lines or "make lint LINTFLAGS = -i".

#	printout makes an enscript -2r printout of SOURCES and
#	and INCFILES.  Use lpr if you don't have transcript.
LPR = enscript -2r
# LPR = lpr
printout:
	$(LPR) $(SOURCES) $(INCFILES)

#	Finally, to clean things up (whenever we want to remake
#	everything, for example) we "make clean" using
clean:
	rm -f $(PROGRAM) $(OBJECTS) $(LINTFILES)







