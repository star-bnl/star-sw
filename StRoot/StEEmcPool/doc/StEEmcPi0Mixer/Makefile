TOP=StRoot/StEEmcPool/doc/StEEmcPi0Mixer


all:
	doxygen $(TOP)/mixer.cfg


print:
	cd doc/latex; \
	latex refman ;\
	latex refman ;\
	dvips -o refman.ps refman ;\
	lpr refman.ps

clean:
	rm -rf doc/html doc/latex 
