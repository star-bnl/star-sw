//C...HEPEVT commonblock.
#define NMXHEP 4000
struct HEPEVT
{ long   NEVHEP,NHEP,ISTHEP[NMXHEP],IDHEP[NMXHEP],
         JMOHEP[NMXHEP][2],JDAHEP[NMXHEP][2];
  double PHEP[NMXHEP][5],VHEP[NMXHEP][4];
} hepevt_;

struct HEPEVTD
{ long  lmxhep,nbytespw,irunnum;
  char  cevgen[8];
} hepevtd_;
