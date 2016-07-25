void UpdateTpcSecRowB(const TH1 *hist, 
		      const Char_t *Name="dEdx/StarDb/Calibrations/tpc/TpcSecRowB.20030106.000000.root.Hist536"){
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable"); 
  if (gClassTable->GetID("St_TpcSecRowCor") < 0) gSystem->Load("St_Tables"); 
  //  const TH1 *hist = (TH1 *) gDirectory->Get("mu");  
  if (! hist) return;
  TFile *old = new TFile(Name); 
  St_TpcSecRowCor *secrow = (St_TpcSecRowCor *) old->Get("TpcSecRowB");
  TpcSecRowCor_st *gain = secrow->GetTable();
  Int_t NbinsX = hist->GetNbinsX(); cout << "NbinsX = " << NbinsX << endl;
  Int_t NbinsY = hist->GetNbinsY(); cout << "NbinsY = " << NbinsY << endl;
  TpcSecRowCor_st *row = 0;
  for (Int_t i=1; i<=NbinsX; i++) {
    row = gain + i - 1;
    for (Int_t j=1; j<= NbinsY; j++) {
      cout << "sector " << i << "\trow " << j << "\tgain " << row->GainScale[j-1];
      row->GainScale[j-1] *= TMath::Exp(-hist->GetBinContent(i,j));
      cout << "\t=>\t" << row->GainScale[j-1] << endl;
    }
  }
  secrow->Print(0,24);
  TString filename(Name);
  filename  += ".new";
  printf("Create %s\n",filename.Data());
  TFile *f = new TFile(filename.Data(),"recreate");
  secrow->Write();
  delete f;
  delete old;

}
