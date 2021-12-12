#ifdef __TFG__VERSION__
#include <assert.h>
#endif /* __TFG__VERSION__ */
#include "StMuPrimaryTrackCovariance.h"
#include "TMath.h"
#include "Stiostream.h"
#include "TRSymMatrix.h"
#ifndef __TFG__VERSION__
ClassImp(StMuPrimaryTrackCovariance);
#else /* __TFG__VERSION__ */

static Int_t _debug = 0;
#endif /* __TFG__VERSION__ */
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
#ifdef __TFG__VERSION__
  if (_debug) {
    TRSymMatrix C(3,covv); cout << "StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance\t" << C << endl;
  }
  assert(mTanTan > 0 && mPsiPsi > 0 && mPtiPti > 0);
#endif /* __TFG__VERSION__ */
}
//________________________________________________________________________________
#ifndef __TFG__VERSION__
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance(const Float_t *cov) {
#else /* __TFG__VERSION__ */
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance(const TArrayF &cov) {
#endif /* __TFG__VERSION__ */
  memset(mBeg,0,mEnd-mBeg+1);
#ifndef __TFG__VERSION__
  mTanTan = cov[ 9];                                                                         
  mPsiTan = cov[10]*TMath::DegToRad(); mPsiPsi = cov[12]*TMath::DegToRad()*TMath::DegToRad();
  mPtiTan = cov[11]                  ; mPtiPsi = cov[13]*TMath::DegToRad()                  ; mPtiPti = cov[14];
#else /* __TFG__VERSION__ */
  if (cov.GetSize() == 15) {
    mTanTan = cov[ 9];     
    mPsiTan = cov[10]*TMath::DegToRad(); mPsiPsi = cov[12]*TMath::DegToRad()*TMath::DegToRad();
    mPtiTan = cov[11]                  ; mPtiPsi = cov[13]*TMath::DegToRad()                  ; mPtiPti = cov[14];
  } else {
    if (cov.GetSize() >= 9) 
    memcpy(&mTanTan, cov.GetArray()+3, 6*sizeof(Float_t));
  }
  if (_debug) {
    if (cov.GetSize() == 15) {
      TRSymMatrix C(5,cov.GetArray()); cout << "StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance\t" << C << endl;
    } else {
      TRSymMatrix C(3,cov.GetArray()+3); cout << "StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance\t" << C << endl;
    }
    cout << "*this\t" << *this << endl;
  }
  assert(mTanTan >= 0 && mPsiPsi >= 0 && mPtiPti >= 0);
#endif /* __TFG__VERSION__ */
}
ostream&  operator<<(ostream& os, const StMuPrimaryTrackCovariance& v) {
  TRSymMatrix cov(3,v.errMatrix());
  os << "StMuPrimaryTrackCovariance: " << cov;
  return os;
}
void StMuPrimaryTrackCovariance::Print(Option_t *option) const {cout << *this << endl;}
