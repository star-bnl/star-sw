#ifndef StMagF_H
#define StMagF_H
#include "Rtypes.h"
#include "TNamed.h"
#ifndef __CINT__
#include "StarCallf77.h" 
#define gufld  F77_NAME(gufld,GUFLD)
R__EXTERN  "C" {
  void type_of_call gufld(Float_t *x, Float_t *bf);
}
#endif
class StMagF : public TNamed {
public:
  static void Agufld(float *x, float *b) {gufld(x,b);}
  ClassDef(StMagF,1)
};
#endif
