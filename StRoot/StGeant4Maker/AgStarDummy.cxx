// Provide dummy functions expected from starsim
#include "StarCallf77.h"

extern "C" {
  void type_of_call agsvert_( float *vertex, int *nb, int *nt, float *ubuf, int *nu, int *nv ) { }
  void type_of_call agskine_( float *plab,   int *ip, int *nv, float *ubuf, int *nb, int *nt ) { }
  void type_of_call  gsvert_( float *, int &, int &, float *, int &, int &){ }
  void type_of_call  gskine_( float *, int &, int &, float *, int &, int &){ }
  void type_of_call gcomad_(DEFCHARD, int*& DEFCHARL){ }
};
