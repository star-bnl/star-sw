TH2D *Rebin(TH2 *hist, Float_t factor=0.1) {
  if (!hist) return 0;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  TString name(hist->GetName());
  name += "_rb";
  Int_t Nx = nx*factor; 
  Int_t Ny = ny*factor; printf("Rebin %i x %i -> %i x %i\n",nx,ny,Nx,Ny);
  TH2D *h = new TH2D(name.Data(),hist->GetTitle(),
		     Nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
		     Ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  for (int i=1;i<=nx;i++)
    for (int j=1;j<=ny;j++)
      h->Fill( hist->GetXaxis()->GetBinCenter(i),
	       hist->GetYaxis()->GetBinCenter(j),
	       hist->GetCellContent(i,j)*factor*factor);
  return h;
}
