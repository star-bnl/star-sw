TH1D *MakeGraphFromAscii(Char_t *FileName="dNdx_Bichsel.data") {
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FILE *fp = fopen(FileName,"r");
  const Int_t N = 500;
  Int_t     i;
  Double_t  x[N];
  Double_t  y[N];
  Double_t  z[N];
  Float_t  X,Y;
  Int_t npoints = 0;
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%i %f %f",&i,&X,&Y);
    //    printf("%s",line);
    x[npoints] = X;
    y[npoints] = Y;
    printf("%i\t%f\t%f\n",i,x[npoints],y[npoints]);
    npoints++;
    if (npoints > N) break;
  }
  fclose(fp);
  Double_t *xBins = new Double_t [npoints+1];
  for (i = 0; i <= npoints; i++) {
    if (i == 0)            xBins[i] = x[i]   - 0.5*(x[i+1] - x[i]); 
    else if (i == npoints) xBins[i] = x[i-1] + 0.5*(x[i-1] - x[i-2]);
      else                 xBins[i] = 0.5*(x[i] + x[i-1]);
    cout << i << "\t" << x[i] << "\t" << xBins[i] << endl;
  }
  TH1D *hist = new TH1D("dNdx","dNdx versus #beta#gamma",npoints,xBins);
  for (i = 0; i < npoints; i++) hist->SetBinContent(i+1,y[i]);
  hist->SetLineColor(2);
  hist->SetLineWidth(4);
  hist->SetMarkerColor(4);
  hist->SetMarkerStyle(21);
  hist->GetXaxis()->SetTitle("#beta#gamma");
  hist->GetYaxis()->SetTitle("dN/dx (collision/cm)");
  hist->Draw("ACP");
  f->Write();
  return hist;
}
