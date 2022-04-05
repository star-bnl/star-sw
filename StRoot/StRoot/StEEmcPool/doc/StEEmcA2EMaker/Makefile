TOP=StRoot/StEEmcPool/doc/StEEmcA2EMaker


all:
	doxygen $(TOP)/a2e.cfg


print:
	cd doc/latex; \
	latex refman ;\
	latex refman ;\
	dvips -o refman.ps refman ;\
	lpr refman.ps

clean:
	rm -rf doc/html doc/latex 
