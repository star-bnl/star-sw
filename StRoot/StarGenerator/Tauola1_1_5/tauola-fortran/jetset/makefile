include ../make.inc
COMMAND = 
COMMAND_OBJECTS = 
LOCAL_LIB   =  jetset.a 
LIB_OBJECTS =  jetset74.o
####################################################
#-------  What has to be built 
all:	$(LOCAL_LIB)
####################################################
jetset74.f:	../include/HEPEVT.h
####################################################
.f.o:
	$(F77) $(FFLAGS) -c $<
$(LOCAL_LIB): $(LIB_OBJECTS)
	$(AR) rv  $(LOCAL_LIB) $(LIB_OBJECTS)
clean:
	(rm -f *.o; rm -f *.a; rm -f *~ lmap)
