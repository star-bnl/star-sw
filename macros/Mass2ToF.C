/*
  MuDst->Draw("PrimaryTracks.mP.magnitude()**2*(1./PrimaryTracks.mBTofPidTraits.mBeta**2-1)>>m2(100,0,1)","PrimaryTracks.mBTofPidTraits.mBeta>0","colz")
  .L Mass2ToF.C+
  MuDst->Draw("Mass2ToF(PrimaryTracks.mBTofPidTraits.beta(),PrimaryTracks.mP.magnitude())","PrimaryTracks.mBTofPidTraits.beta()>-1");
  root.exe -q -b -x  'lMuDst.C(0,"VMC.MuDst.root")' Mass2ToF.C+
 */
void Mass2ToF() {
  TChain *tree = StMuDstMaker::instance()->chain();
  if (! tree) return;
  Long64_t nentries = tree->GetEntries();
  for (Long64_t i_evt = 0; i_evt < nentries; i_evt++) {
  return p*p*(1. - 1./(beta*beta));
}
