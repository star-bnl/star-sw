/*
   MuDst->Draw("ToFMass2(PrimaryTracks.mP.mX1,PrimaryTracks.mP.mX2,PrimaryTracks.mP.mX3,PrimaryTracks.mBTofPidTraits.mBeta):PrimaryTracks.mPt>>M2(100,0,5,150,-0.05,1.45)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
*/

#include "Rtypes.h"
#include "Riostream.h"
Double_t ToFMass2(Float_t px, Float_t py, Float_t pz, Float_t beta) {
  if (beta <= 0 || beta > 1.0) return -1.;
  Double_t mass2 = (px*px + py*py + pz*pz)*(1./(beta*beta) - 1);
  //  cout << "px/y/z = " << px << " / " << py << " / " << pz << " beta = " << beta << " mass2 = " << mass2 << endl;
  return mass2;
}
