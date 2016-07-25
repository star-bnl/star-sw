TProfile2D *Prof3(TH3 *hist) {
  if (!hist) return 0;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  Int_t nz = hist->GetNbinsZ();
  TString name(hist->GetName());
  name += "_pxy";
  printf("Prof3 %i x %i x %i\n",nx,ny,nz);
  TProfile2D *h = new TProfile2D (name.Data(),hist->GetTitle(),
		     nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
		     ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  for (int i=1;i<=nx;i++)
    for (int j=1;j<=ny;j++)
      for (int k=1; k<=nz; k++)
	h->Fill( hist->GetXaxis()->GetBinCenter(i),
		 hist->GetYaxis()->GetBinCenter(j),
		 hist->GetZaxis()->GetBinCenter(k),
		 hist->GetBinContent(i,j,k));
  return h;
}
