#include "TMath.h"
#include "Riostream.h"
#include "TString.h"
//________________________________________________________________________________
Double_t  dEdxResolution(Double_t N, Double_t P, Double_t dx, Double_t alpha=0.46) {
  // STAR CDR Table 4C-8, page 4C-33
  Double_t L = dx*N;
  Double_t res = 100*0.47*TMath::Power(N,-alpha)*TMath::Power(P*dx,-0.32);
  cout << "dEdxResolution(N = " << N << ", P = " << P << ",h = " << dx << ", alpha = " << alpha << ", L = " << L << ") = " << Form("%4.1f",res) << "\%" << endl;
  return res;
}
//________________________________________________________________________________
void dEdxResolution() {
  Double_t alphaAllison = 0.46;
  Double_t alphaWalenta = 0.43;
  cout << "W.W.M.Allison: alpha = " << alphaAllison << endl;
  Double_t resInner, resOuter, resiTPC, sigmaTPC, sigmaiTPC;
  resInner  = dEdxResolution(13,1.,1.15,alphaAllison); //cout << "Inner TPC resolution = " << resInner << endl;
  resOuter  = dEdxResolution(32,1.,1.95,alphaAllison); //cout << "Outer TPC resolution = " << resOuter << endl;
  resiTPC   = dEdxResolution(40,1.,1.55,alphaAllison); //cout << "iTPC TPC resolution = " << resiTPC << endl;
  sigmaTPC  = 1./TMath::Sqrt(1./(resInner*resInner) + 1./(resOuter*resOuter)); cout << "Old TPC = " << sigmaTPC << endl;
  sigmaiTPC = 1./TMath::Sqrt(1./(resiTPC*resiTPC) + 1./(resOuter*resOuter));   cout << "New TPC = " << sigmaiTPC << endl;
  cout << "A.H.Walena: alpha = '" << alphaWalenta << endl;
  resInner  = dEdxResolution(13,1.,1.15,alphaWalenta); //cout << "Inner TPC resolution = " << resInner << endl;
  resOuter  = dEdxResolution(32,1.,1.95,alphaWalenta); //cout << "Outer TPC resolution = " << resOuter << endl;
  resiTPC   = dEdxResolution(40,1.,1.55,alphaWalenta); //cout << "iTPC TPC resolution = " << resiTPC << endl;
  sigmaTPC  = 1./TMath::Sqrt(1./(resInner*resInner) + 1./(resOuter*resOuter)); cout << "Old TPC = " << sigmaTPC << endl;
  sigmaiTPC = 1./TMath::Sqrt(1./(resiTPC*resiTPC) + 1./(resOuter*resOuter));   cout << "New TPC = " << sigmaiTPC << endl;
}
