# Makefile jml - 3/9/99
# CFLAGS = -g -D_REENTRANT
# CFLAGS = -g -D__EXTENSIONS__ -D_POSIX_C_SOURCE=199506L
INCL_DIRS = -I../../  -I../ -I/usr/local/include/g++
CFLAGS = -g $(INCL_DIRS)

IFACE_DIR = GENERIC
IMPL_DIR = TPC

client:	client.o EventReader.o DetectorReader.o TPCV1P0.o TPCV1P0_ZS_SR.o TPCV1P0_ADCR_SR.o TPCV1P0_CPP_SR.o TPCV2P0.o TPCV2P0_ZS_SR.o TPCV2P0_ADCR_SR.o TPCV2P0_CPP_SR.o CRC.o RecHeaderFormats.o TPCV1P0.Banks.o TPCV2P0.Banks.o  swaps.o MemMan.o
	g++ $(CFLAGS) client.o EventReader.o DetectorReader.o TPCV1P0.o TPCV1P0_ZS_SR.o TPCV1P0_ADCR_SR.o TPCV1P0_CPP_SR.o TPCV2P0.o TPCV2P0_ZS_SR.o TPCV2P0_ADCR_SR.o TPCV2P0_CPP_SR.o CRC.o RecHeaderFormats.o  TPCV1P0.Banks.o TPCV2P0.Banks.o swaps.o MemMan.o -o client

client.o:	client.cxx
	g++ $(CFLAGS) -c client.cxx

EventReader.o:	$(IFACE_DIR)/EventReader.hh $(IFACE_DIR)/EventReader.cxx
	g++ $(CFLAGS) -c $(IFACE_DIR)/EventReader.cxx

DetectorReader.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/DetectorReader.cxx $(IMPL_DIR)/TPCV1P0.hh
	g++ $(CFLAGS) -c $(IMPL_DIR)/DetectorReader.cxx

TPCV1P0.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV1P0.hh $(IMPL_DIR)/TPCV1P0.cxx 
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV1P0.cxx

TPCV1P0_ZS_SR.o: 	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV1P0.hh $(IMPL_DIR)/TPCV1P0_ZS_SR.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV1P0_ZS_SR.cxx

TPCV1P0_ADCR_SR.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV1P0.hh $(IMPL_DIR)/TPCV1P0_ADCR_SR.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV1P0_ADCR_SR.cxx

TPCV1P0_CPP_SR.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV1P0.hh $(IMPL_DIR)/TPCV1P0_CPP_SR.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV1P0_CPP_SR.cxx

CRC.o:	$(IFACE_DIR)/CRC.hh $(IFACE_DIR)/CRC.cxx $(IFACE_DIR)/RecHeaderFormats.hh
	g++ $(CFLAGS) -c $(IFACE_DIR)/CRC.cxx

RecHeaderFormats.o:	$(IFACE_DIR)/RecHeaderFormats.hh $(IFACE_DIR)/RecHeaderFormats.cxx
	g++ $(CFLAGS) -c $(IFACE_DIR)/RecHeaderFormats.cxx

TPCV1P0.Banks.o:	$(IFACE_DIR)/RecHeaderFormats.hh $(IMPL_DIR)/TPCV1P0.Banks.hh $(IMPL_DIR)/TPCV1P0.Banks.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV1P0.Banks.cxx

swaps.o:	$(IFACE_DIR)/swaps.hh $(IFACE_DIR)/swaps.cxx
	g++ $(CFLAGS) -c $(IFACE_DIR)/swaps.cxx

MemMan.o:	$(IMPL_DIR)/MemMan.hh $(IMPL_DIR)/MemMan.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/MemMan.cxx

TPCV2P0.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV2P0.hh $(IMPL_DIR)/TPCV2P0.cxx 
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV2P0.cxx

TPCV2P0_ZS_SR.o: 	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV2P0.hh $(IMPL_DIR)/TPCV2P0_ZS_SR.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV2P0_ZS_SR.cxx

TPCV2P0_ADCR_SR.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV2P0.hh $(IMPL_DIR)/TPCV2P0_ADCR_SR.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV2P0_ADCR_SR.cxx

TPCV2P0_CPP_SR.o:	$(IFACE_DIR)/EventReader.hh $(IMPL_DIR)/TPCV2P0.hh $(IMPL_DIR)/TPCV2P0_CPP_SR.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV2P0_CPP_SR.cxx

TPCV2P0.Banks.o:	$(IFACE_DIR)/RecHeaderFormats.hh $(IMPL_DIR)/TPCV2P0.Banks.hh $(IMPL_DIR)/TPCV2P0.Banks.cxx
	g++ $(CFLAGS) -c $(IMPL_DIR)/TPCV2P0.Banks.cxx


# DO NOT DELETE THIS LINE -- make depend depends on it.





