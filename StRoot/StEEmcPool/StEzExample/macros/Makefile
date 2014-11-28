SUBDIRS= StRoot/StDbLib \
	 StRoot/StEEmcDbMaker \
	 StRoot/StEEmcUtil/EEfeeRaw \
	 StRoot/StEEmcPool/StEzExample \
         EEmcDb


all:
	@for i in $(SUBDIRS); do \
	echo "make  in $$i" ;    \
	cd $$i ; $(MAKE) ; \
	if [ $$? -ne 0 ]; then break; fi ;\
	cd -; \
	done

clean:
	echo "clean: $(SUBDIRS)"
	for i in $(SUBDIRS); do echo "make clean in $$i" ; cd $$i; $(MAKE) clean ; cd -; done

