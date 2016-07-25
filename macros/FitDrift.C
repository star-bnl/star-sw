class St_TpcDriftDistCorr;
St_TpcDriftDistCorr *FitDrift(Int_t k=0){
  TFile *fRootFile = (TFile *) gDirectory->GetFile();
  if (! fRootFile ) {printf("Cannot find/open %s",fRootFile->GetName()); return;}
  TH2F *Z_xy = (TH2F *) fRootFile->Get("Z_xy");
  if (!Z_xy) {printf("Cannot find Z_xy\n"); return;}
  Int_t nx = Z_xy->GetNbinsX(); printf ("nx = %i\n",nx);
  Double_t xlow = Z_xy->GetXaxis()->GetXmin();
  Double_t xup  = Z_xy->GetXaxis()->GetXmax();
  TH1D *proj=0;
  //  TF1 *powe= new TF1("powe","pol2",0,190);
  TF1 *powe = new TF1("powe","[0]+x*([1]+x*[2])",0,185);
  St_TpcDriftDistCorr *drift = new St_TpcDriftDistCorr("TpcDriftDistCorr",48);
  TpcDriftDistCorr_st row;
  for (int i=0;i<nx;i++) {
    if (k == 0 || k != 0 && k == i) {
      cout << "Bin " << i << "---------" << endl;
      char line[20];
      sprintf(line,"%s%02i",Z_xy->GetName(),i);
      TString name(line);
      proj = Z_xy->ProjectionY(name.Data(),i,i,"e");
      proj->Fit("powe","R"); 
      for (int j=0; j<4; j++) {
	row.a[j] = powe->GetParameter(j);
      }
      drift->AddAt(&row,i);
      //    delete proj;
    }
  }
  return drift;;
}
