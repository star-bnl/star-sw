#ifndef StMagF_H
#define StMagF_H
#include "Rtypes.h"
#include "StarCallf77.h" 
#define Agufld F77_NAME(gufld,GUFLD)
#define Gufld Agufld
R__EXTERN  "C" {
  void type_of_call Agufld(Float_t *x, Float_t *bf);
}
#endif
