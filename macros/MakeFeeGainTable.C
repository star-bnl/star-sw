void MakeFeeGainTable(){
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
  gSystem->Load("St_base");
  gSystem->Load("St_Tables");
  St_tpcFeeGainCor *gain = new St_tpcFeeGainCor("tpcFeeGain",24);
  gROOT->LoadMacro("dEdxFit.C");
  MakeTable(gain);
  f = new TFile("tpcFeeGain.20000614.175430.root","recreate");
  gain->Write();
  delete f;
}
