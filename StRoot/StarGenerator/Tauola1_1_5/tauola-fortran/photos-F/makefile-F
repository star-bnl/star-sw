# platform - dependant files
include ../make.inc
###########################################################
LOCAL_LIB   =  glib.a 
LIB_OBJECTS =  photos.o 
##############   What has to be built
all:	$(LOCAL_LIB) 
AR = ar
RANLIB = ranlib
CPPP = $(CPP)  $(CPPFLAGS)
#############
.f.o:
	$(F77) $(FFLAGS) -c $<
$(LOCAL_LIB): $(LIB_OBJECTS)
	$(AR) rv  $(LOCAL_LIB) $(LIB_OBJECTS)
	$(RANLIB) $(LOCAL_LIB)
clean:
	@rm -f *.o; rm -f *.a; rm -f *~
	(cd ./demo; make clean)

