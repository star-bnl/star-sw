void PrintTH2D(TH2D *hist1, TH2D *hist2) {
  if (! hist1 || ! hist2) {cout << "empty histos" << endl; return;}
  Int_t nx = hist1->GetNbinsX();
  Int_t ny = hist1->GetNbinsY();
  if (nx != hist2->GetNbinsX() || ny != hist2->GetNbinsY()) { cout << "Incompartible histograms" << endl; return;}
  TString name(hist1->GetName());
  name += "Diff";
  TH2D *Diff = new TH2D(*hist1);
  Diff->SetName(name);
  Diff->Reset();
  Double_t emax = 0;
  Double_t zmax, rmax;
  for (int i=1;i<=nx;i++){
    for (int j=1;j<=ny;j++){
      Double_t v1 = hist1->GetCellContent(i,j);
      Double_t v2 = hist2->GetCellContent(i,j);
      Double_t x = hist1->GetXaxis()->GetBinCenter(i);
      Double_t y = hist1->GetYaxis()->GetBinCenter(j);
      Double_t v = TMath::Abs(v1+v2);
      Double_t e = TMath::Abs(v1 - v2)/(v + 1);
      Diff->Fill(x,y,v1-v2);
      if (e > 1e-2) {
	cout << i << "\t" << j << "\tz " << x << "\tr " << y
	     << "\tv1 " << v1 << "\tv2 " << v2 << "\tdv " << v1 - v2 
	     << "\te " << e << endl;
	if (emax < e) {
	  emax = e;
	  zmax = x;
	  rmax = y;
	}
      }
    }
  }
  if (emax > 0) {
    cout << "max deviation " << emax << "\tat z " << zmax << "\tr " <<rmax << endl;
  }
}
