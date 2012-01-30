#ifndef StMagF_H
#define StMagF_H
#include "Rtypes.h"
#include "TNamed.h"
#include "StarMagField/StarMagField.h"
#ifndef __CINT__
#include "StarCallf77.h" 
#define gufld  F77_NAME(gufld,GUFLD)
R__EXTERN  "C" {
  void type_of_call gufld(Float_t *x, Float_t *bf);
}
#endif
class StMagF : public TNamed {
public:
  static void Agufld(float *x, float *b);
ClassDef(StMagF,0) //mmmm
};
inline void StMagF::Agufld(float *x, float *b) {
  static StarMagField *mag = StarMagField::Instance();
  double xx[3]={x[0],x[1],x[2]}, bb[3];
  mag->BField(xx,bb);
  b[0]=bb[0]; b[1]=bb[1]; b[2]=bb[2];
}
#endif
