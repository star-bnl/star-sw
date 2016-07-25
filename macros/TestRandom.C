void TestRandom() {
  const Int_t NX = 20;
  TH1F *hist = new TH1F("hist","histo from a gaussian",NX,-3,3);
  hist->FillRandom("gaus",10000);
  hist->ComputeIntegral();
  Double_t *fInt = hist->GetIntegral();
  const Int_t Nx = 20;
  Double_t x[Nx+1], y[Nx+1];
  int i;
  for (i = 0; i<=Nx; i++) {
    Double_t r=i; r /= Nx;
    Int_t ibin = TMath::BinarySearch(NX,fInt,r);
    if (ibin < 1) ibin = 1;
    if (ibin > NX - 1) ibin = NX - 1;
    x[i] = hist->GetBinLowEdge(ibin)
      +hist->GetBinWidth(ibin+1)*(r-fInt[ibin])/(fInt[ibin+1] - fInt[ibin]);
    //Axis_t TH1::GetRandom()
//    Int_t ibin = TMath::BinarySearch(nbinsx,&fIntegral[0],r1);
//    return GetBinLowEdge(ibin+1)
//       +GetBinWidth(ibin+1)*(fIntegral[ibin+1]-r1)/(fIntegral[ibin+1] - fIntegral[ibin]);
    y[i] =  hist->GetBinLowEdge(ibin+1)
      +hist->GetBinWidth(ibin+1)*(fInt[ibin+1]-r)/(fInt[ibin+1] - fInt[ibin]);
    if (i == 0)
      printf("i\tibin\t      x\t             y\t         r\t       Int\n");
    printf("%i\t%i\t%f\t%f\t%f\t%f",i,ibin,x[i],y[i],r,fInt[ibin]);
    if (i > 0) {
      if (y[i] < y[i-1]) printf(" <   yyyyyyyyyy");
      if (x[i] < x[i-1]) printf(" <   xxxxxxxxxx");
    }
    printf("\n");
  }
}
