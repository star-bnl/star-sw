/*
  MuDst->Draw("PrimaryTracks.mP.magnitude()**2*(1./PrimaryTracks.mBTofPidTraits.mBeta**2-1):PrimaryTracks.mP.magnitude()*PrimaryTracks.charge()>>m2(100,-5,5,100,0,1)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
  MuDst->Draw("PrimaryTracks.mP.magnitude()**2*(1./PrimaryTracks.mBTofPidTraits.mBeta**2-1):PrimaryTracks.pt()*PrimaryTracks.charge()>>m2(100,-5,5,100,0,1)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
 */
Double_t Mass2ToF(Float_t beta = 0, Float_t p = 0) {
  if (beta <= 0) return 0;
  return p*p*(1. - 1./(beta*beta));
}
