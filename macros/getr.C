void getr(const TH1 *hist,const Int_t nx=0) {
  Int_t Nx = nx;
  Int_t NX = hist->GetNbinsX();
  if (!Nx) Nx = NX;
  hist->ComputeIntegral();
  Double_t *fInt = hist->GetIntegral();
  Double_t x[1000];
  x[0]  = hist->GetXaxis()->GetXmin();
  x[Nx] = hist->GetXaxis()->GetXmax();
  int i;
  for (i=1; i<Nx; i++) {
    Double_t r=i; r /= Nx;
    Int_t ibin = TMath::BinarySearch(NX,fInt,r);
    x[i] = hist->GetBinLowEdge(ibin)
      +hist->GetBinWidth(ibin+1)*(r-fInt[ibin])/(fInt[ibin+1] - fInt[ibin]);
    Double_t p = TMath::Power(10.,x[i]);
    printf("%f\t%i\t%f\t%f\t%f\n",i,ibin,x[i],p,fInt[ibin]);
    //    cout << i << "\t" << r << "\t" << ibin << "\t" << x[i] << "\t" << p << "\t" << fInt[ibin] << endl;
  }
  printf("Double_t  x[] = {\n");
  for (i = 0; i <= Nx; i++) {
    printf("%6.2f,",TMath::Power(10.,x[i]));
    if ((i+1)%10 == 0) printf("\n");
  }
  printf("};\n");
}
