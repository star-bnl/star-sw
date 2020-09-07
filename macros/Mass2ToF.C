/*
  MuDst->Draw("PrimaryTracks.mP.magnitude()**2*(1./PrimaryTracks.mBTofPidTraits.mBeta**2-1):PrimaryTracks.mP.magnitude()*PrimaryTracks.charge()>>m2(100,-5,5,100,0,1)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
  MuDst->Draw("PrimaryTracks.mP.magnitude()**2*(1./PrimaryTracks.mBTofPidTraits.mBeta**2-1):PrimaryTracks.pt()*PrimaryTracks.charge()>>m2(100,-5,5,100,0,1)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
 */
Double_t Mass2ToF(Float_t beta, Float_t p) {
  return p*p*(1. - 1./(beta*beta));
}
