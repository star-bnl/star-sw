#include "TMath.h"
#include "Riostream.h"
#include "TString.h"
Double_t  dEdxResolution(Double_t N = 32, Double_t P = 1, Double_t dx = 2) {
  // STAR CDR Table 4C-8, page 4C-33
  Double_t res = 100*0.47*TMath::Power(N,-0.46)*TMath::Power(P*dx,-0.32);
  cout << "dEdxResolution(N=" << N << ",P=" << P << ",h=" << dx << ") = " << res << "\%" << endl;
  return res;
}
