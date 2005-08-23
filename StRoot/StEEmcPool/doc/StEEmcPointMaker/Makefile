TOP=StRoot/StEEmcPool/doc/StEEmcPointMaker


all:
	doxygen $(TOP)/points.cfg


print:
	cd doc/latex; \
	latex refman ;\
	latex refman ;\
	dvips -o refman.ps refman ;\
	lpr refman.ps

clean:
	rm -rf doc/html doc/latex 
