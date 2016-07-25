/*
  MuDst->Draw("PrimaryTracks.mP.magnitude()**2*(1./PrimaryTracks.mBTofPidTraits.mBeta**2-1)>>m2(100,0,1)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
 */
Double_t MassToF(Float_t beta, Float_t p) {
  return p*p*(1. - 1./(beta*beta));
}
