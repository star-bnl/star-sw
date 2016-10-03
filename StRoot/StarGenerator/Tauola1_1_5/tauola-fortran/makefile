Clean:  
	rm -f *.o; rm -f *.a; rm -f *~; rm -f *.exe
	(cd ./glibk;	make clean)
	(cd ./jetset;	make clean)
	(cd ./jetset2;	make clean)
	(cd ./photos-F;	make clean)
	(cd ./tauola-F;	make clean)
move: 
	rm -f *.o; rm -f *.a; rm -f *~; rm -f *.exe
	(cp tauola/demo-pythia/README eli/.)
	(cp tauola/demo-pythia/demo.f eli/.)
	(cp tauola/demo-pythia/makefile eli/.)
	(cp tauola/demo-pythia/tauface-jetset.f eli/.)
	(cd ./glibk;	make clean)
	(cd ./jetset;	make clean)
	(cd ./jetset2;	make clean)
	(cd ./photos-F;	make clean)
	(cd ./tauola-F;	make clean)

	(cd ../; \
	tar -cvzf TAUOLA.tar.gz TAUOLA )
export: Clean
	(cd ../; \
	tar -cvzf TAUOLA-exp-11-10-2005.tar.gz TAUOLA/README TAUOLA/README-updates TAUOLA/eli TAUOLA/glibk TAUOLA/include TAUOLA/jetset TAUOLA/jetset2 TAUOLA/makefile TAUOLA/photos-F TAUOLA/tauola-F TAUOLA/tauola-BBB  TAUOLA/platform TAUOLA/make.inc TAUOLA/randg TAUOLA/demo-factory TAUOLA/tauola-factory TAUOLA/doc/TAUOLA-F.ps TAUOLA/doc/v32p1277.ps.gz TAUOLA/doc/0201149.ps.gz TAUOLA/doc/v33p1875.ps.gz TAUOLA/doc/CERN-TH-2003-287.ps.gz TAUOLA/doc/fp3.ps.gz)






