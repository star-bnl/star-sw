DCAPlot(const char* inFile, const char* outdir)
{

  
  //make some strings with the cuts so I don't have to keep writing them, and
  //so they're consistent
  
  TString MCCutString = "abs(mMcTracks.mEtaMc)<.5 && abs(mMcTracks.mChargeMc)>0 && mMcTracks.mPtMc>.3 && mMcTracks.mNHitMc>10 && mMcTracks.mNHitMc<55";
  TString RecCutString = "abs(mMatchedPairs.mEtaMc)<.5 && abs(mMatchedPairs.mChargeMc)>0 && mMatchedPairs.mPtMc>.3 && mMatchedPairs.mNHitMc>10 && mMatchedPairs.mNHitMc<55";

  TFile *nFle = new TFile(inFile);

  TH1D *dcaGl = new TH1D("dcaGl","",50,0.,5.);
  TH1D *dcaPr = new TH1D("dcaPr","",50,0.,5.);


  TCanvas *c1 = new TCanvas("c1","",400,300);
  StMiniMcTree->Draw("mMatchedPairs.mDcaGl>>dcaGl",RecCutString);
  c1->SaveAs("dcaGl.ps");
  StMiniMcTree->Draw("mMatchedPairs.mDcaPr>>dcaPr",RecCutString);
  c1->SaveAs("dcaPr.ps");


  nFle->Close();



}
