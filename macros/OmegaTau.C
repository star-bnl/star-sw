#include "TMath.h"
Double_t OmegaTau(Double_t *b, Double_t *p=0) {
  Double_t omegatau = 0;
  Double_t B = b[0];
  Double_t B2 = B*B;
  if (B2 <= 2.143) omegatau = TMath::Sqrt(0.51848343)*B;
  else if (B2 <= 6.4286) omegatau = TMath::Sqrt(0.34075)*B;
  else if (B2 <= 12.14286) omegatau = TMath::Sqrt(0.325)*B;
  else omegatau = TMath::Sqrt(0.22222)*B;
  //  printf("b = %f ot = %f\n",B,omegatau);
  return omegatau;
}
