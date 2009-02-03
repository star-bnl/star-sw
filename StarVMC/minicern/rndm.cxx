#include "TRandom.h"
#include "TMath.h"
extern "C" {
  //  if (! gRandom) new TRandom();
  Float_t rndm_(Int_t i = 0) {    return gRandom->Rndm(i);  }
  Float_t ranf_()            {    return rndm_(0);  }
  Float_t freq_ (Float_t x)  {    return TMath::Freq(x); }

#ifdef  __GNUC__
#define C_RAD_PER_DEG  0.017453292519943295769237
#define C_DEG_PER_RAD 57.295779513082320876798155
  Double_t dsind_(Double_t degree) {return TMath::Sin (C_RAD_PER_DEG*degree);}
  Double_t dcosd_(Double_t degree) {return TMath::Cos (C_RAD_PER_DEG*degree);}
  Double_t dtand_(Double_t degree) {return TMath::Tan (C_RAD_PER_DEG*degree);}
  Double_t dasind_(Double_t arg)   {return C_DEG_PER_RAD*TMath::ASin (arg);}
  Double_t dacosd_(Double_t arg)   {return C_DEG_PER_RAD*TMath::ACos (arg);}
  Double_t datand(Double_t arg)    {return C_DEG_PER_RAD*TMath::ATan (arg);}
  Double_t datan2d_(Double_t arg1,Double_t arg2) {return  C_DEG_PER_RAD*TMath::ATan2 (arg1,arg2);}
  Float_t sind_(Float_t degree) {return TMath::Sin (C_RAD_PER_DEG*degree);}
  Float_t cosd_(Float_t degree) {return TMath::Cos (C_RAD_PER_DEG*degree);}
  Float_t tand_(Float_t degree) {return TMath::Tan (C_RAD_PER_DEG*degree);}
  Float_t asind_(Float_t arg)   {return C_DEG_PER_RAD*TMath::ASin (arg);}
  Float_t acosd_(Float_t arg)   {return C_DEG_PER_RAD*TMath::ACos (arg);}
#if ! defined( __ICC ) ||  __ICC < 910
  Float_t atand(Float_t arg)    {return C_DEG_PER_RAD*TMath::ATan (arg);}
#endif
  Float_t atan2d_(Float_t arg1,Float_t arg2) {return  C_DEG_PER_RAD*TMath::ATan2 (arg1,arg2);}
#undef C_RAD_PER_DEG
#undef C_DEG_PER_RAD
#endif
  Float_t ran_(Int_t i) {return rndm_(i);}
}
