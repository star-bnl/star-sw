include ../make.inc

SOURCES_F=tauola_extras.f
OBJECTS_F=$(SOURCES_F:.f=.o)

SOURCES_C=f_Decay.c  f_FilHep.c  f_Init.c
OBJECTS_C=$(SOURCES_C:.c=.o)

%.o: %.c
	$(CC) $(CFLAGS) $(DEBUG) -I. -I../tauolaCInterfaces -I../tauolaFortranInterfaces -I../utilities -c $< -o $@

%.o: %.f
	$(F77) $(FFLAGS) $(DEBUG) -I. -c $< -o $@

all: $(OBJECTS_C) $(OBJECTS_F)

clean:
	rm -f *.o; rm -f *~;
