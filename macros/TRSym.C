#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TRVector.h"
#include "TRMatrix.h"
void TRSym() {
  Int_t N = 10;
  Double_t cor = 0.1;
  TArrayD C(N*(N+1)/2);
  for (Int_t i = 0; i < N; i++) {
    Int_t ii = (i+1)*(i+2)/2 - 1;
    C[ii] = 1. - 2*cor;
    Int_t ij = ii - 1;
    if (ij >= 0) C[ij] = cor;
  }
  TRSymMatrix S(N,C.GetArray());
  S.Print();
  TRSymMatrix SI(S,TRArray::kInverted);
  SI.Print();
  TRSymMatrix SR(SI,TRArray::kInverted);
  SR.Print();
}
