include ../make.inc
#######################################################
LOCAL_LIB   =  libpythia.a 
LIB_OBJECTS =  pythia-6.4.20.o
##############   What has to be built
all:	$(LOCAL_LIB) 

.f.o:
	$(F77) $(FFLAGS) -c $<
$(LOCAL_LIB): $(LIB_OBJECTS)
	$(AR) rv  $(LOCAL_LIB) $(LIB_OBJECTS)
#	$(RANLIB) $(LOCAL_LIB)
clean:
	rm -f *.o; rm -f *.a; rm -f *~ 
#######################################################

