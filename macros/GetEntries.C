TH2D *GetEntries(TProfile2D *hist) {
  if (! hist) return 0;
  Int_t nx = hist->GetNbinsX(); 
  Int_t ny = hist->GetNbinsY();
  TAxis *ar = hist->GetXaxis();
  TAxis *az = hist->GetYaxis();
  TString name(hist->GetName());
  name += "_Cont";
  TH2D *newhist = new TH2D(name.Data(),hist->GetTitle(),
			   nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			   ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  for (int i = 1; i <= nx; i++) {
    for (int j = 1; j <= ny; j++) {
      Int_t bin = newhist->GetBin(i,j);
      Double_t v = hist->GetBinEntries(bin);
      newhist->SetBinContent(i,j,v);
    }
  }
  return newhist;
}
