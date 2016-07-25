
#define SETGAIN
{
  TFile *f = 0;

  gROOT->LoadMacro("Load.C");
  Load();
  //  f = new TFile("StarDb/Calibrations/tpc/tpcGain.20000614.175430.root");
  //  f = new TFile("./tpcGain.20000614.175430.root");
  f = new TFile("./StarDb/Calibrations/tpc/tpcFeeGain.20000614.175430.root");
  St_tpcFeeGainCor *gain = (St_tpcFeeGainCor *) f->Get("tpcFeeGain");
  delete f;
  f = new TFile("./tpcFeeGain.root","RECREATE");
  tpcFeeGainCor_st *g = gain->GetTable();
  Int_t N=0, NBad = 0;
  for (int sector=0;sector<gain->GetNRows(); sector++,g++) {
    for (int fee = 0; fee < 182; fee++) {
      for (int i=0; i<2; i++)  g->Gain[fee][i] = 1;
    }
  }
  gain->Write();
  f->Write();
  delete f;
}

