TOP=StRoot/StEEmcPool/doc/TTM


all:
	doxygen $(TOP)/ttm.cfg


print:
	cd doc/latex; \
	latex refman ;\
	latex refman ;\
	dvips -o refman.ps refman ;\
	lpr refman.ps

clean:
	rm -rf doc/html doc/latex 