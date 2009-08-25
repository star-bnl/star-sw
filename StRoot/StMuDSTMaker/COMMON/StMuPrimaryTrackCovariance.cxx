#include "StMuPrimaryTrackCovariance.h"
#include "TMath.h"
#include "Stiostream.h"
#include <cstring>
ClassImp(StMuPrimaryTrackCovariance);
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance() {memset(mBeg,0,mEnd-mBeg+1);}
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance(StMatrixF cov) {
  memset(mBeg,0,mEnd-mBeg+1);
  Float_t *covv = &mTanTan;
  static Double_t deg2rad = TMath::Pi()/180;
  //  cout << cov << endl;
  for (Int_t i = 0; i < 3; i++) {
    for (Int_t j = 0; j <= i; j++) {
      Int_t ij = i*(i+1)/2 + j;
      covv[ij] = cov(i+3,j+3);
      if (i == 1) covv[ij] *= deg2rad;
      if (j == 1) covv[ij] *= deg2rad;
      //      cout << "\t" << covv[ij];
    }
    //    cout << endl;
  }
}
//________________________________________________________________________________
StMuPrimaryTrackCovariance::StMuPrimaryTrackCovariance(const Float_t *cov) {
  memset(mBeg,0,mEnd-mBeg+1);
  static Double_t deg2rad = TMath::Pi()/180;
  mTanTan = cov[ 9];                                                             //cout << mTanTan << endl;
  mPsiTan = cov[10]*deg2rad; mPsiPsi = cov[12]*deg2rad*deg2rad;                  //cout << mPsiTan << "\t" << mPsiPsi << endl;
  mPtiTan = cov[11]        ; mPtiPsi = cov[13]*deg2rad;        mPtiPti = cov[14];//cout << mPtiTan << "\t" << mPtiPsi << "\t" << mPtiPti << endl;
}
