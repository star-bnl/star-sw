#
# Truely a insane makefile
#
# Only use if you're trying to install the executable image int /RTScache
# for operation in the actual DAQ/Online environment
# 

install: 
	cd ~/cvs/; cons; cd ~/cvs/OnlTools/Jevp
	cp Conscript.batty Conscript; cd ~/cvs/; cons; cd ~/cvs/OnlTools/Jevp; rm -f Conscript
	cp launch /RTScache/bin/LINUX/i686
	cp ~/cvs/.sl53_gcc432/lib/* /RTScache/lib/LINUX/i686/STAR_ROOT
	cp launch_reanalyze /RTScache/bin/LINUX/i686