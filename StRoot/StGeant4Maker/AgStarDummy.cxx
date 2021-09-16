// Provide dummy functions expected from starsim
#include "StarCallf77.h"

#define agsvert  F77_NAME(agsvert, AGSVERT)
#define agskine  F77_NAME(agskine, AGSKINE)
#define  gskine	 F77_NAME(gskine,  GSKINE)
#define  gsvert	 F77_NAME(gsvert,  GSVERT)

#define  gcomad	 F77_NAME(gcomad,GCOMAD)

extern "C" {
  void type_of_call agsvert( float *vertex, int *nb, int *nt, float *ubuf, int *nu, int *nv ) { }
  void type_of_call agskine( float *plab,   int *ip, int *nv, float *ubuf, int *nb, int *nt ) { }
  void type_of_call  gsvert( float *, int &, int &, float *, int &, int &){ }
  void type_of_call  gskine( float *, int &, int &, float *, int &, int &){ }
  void type_of_call gcomad(DEFCHARD, int*& DEFCHARL){ }
};
