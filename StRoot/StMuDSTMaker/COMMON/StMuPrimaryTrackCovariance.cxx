#include "StMuPrimaryTrackCovariance.h"
#include "TMath.h"
#include "Stiostream.h"
#include "TRSymMatrix.h"
ClassImp(StMuPrimaryTrackCovariance);
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance() {memset(mBeg,0,mEnd-mBeg+1);}
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance(StMatrixF cov) {
  memset(mBeg,0,mEnd-mBeg+1);
  Float_t *covv = &mTanTan;
  for (Int_t i = 0; i < 3; i++) {
    for (Int_t j = 0; j <= i; j++) {
      Int_t ij = i*(i+1)/2 + j;
      covv[ij] = cov(i+3,j+3);
      if (i == 1) covv[ij] *= TMath::DegToRad();
      if (j == 1) covv[ij] *= TMath::DegToRad();
    }
  }
}
//________________________________________________________________________________
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance(const Float_t *cov) {
  memset(mBeg,0,mEnd-mBeg+1);
  mTanTan = cov[ 9];                                                                         
  mPsiTan = cov[10]*TMath::DegToRad(); mPsiPsi = cov[12]*TMath::DegToRad()*TMath::DegToRad();
  mPtiTan = cov[11]                  ; mPtiPsi = cov[13]*TMath::DegToRad()                  ; mPtiPti = cov[14];
}
ostream&  operator<<(ostream& os, const StMuPrimaryTrackCovariance& v) {
  TRSymMatrix cov(3,v.errMatrix());
  os << "StMuPrimaryTrackCovariance: " << cov;
  return os;
}
void StMuPrimaryTrackCovariance::Print(Option_t *option) const {cout << *this << endl;}
