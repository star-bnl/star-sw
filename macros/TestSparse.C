THnSparseF *sp = 0;
void TestSparse() {
  const Int_t dim = 3;
  const Int_t nbins[dim] = {10, 20, 30};
  const Double_t xmin[dim] = {-10,-20,-30};
  const Double_t xmax[dim] = { 10, 20, 30};
  sp = new THnSparseF("sp","sp",dim,nbins,xmin,xmax);
  Double_t x[dim];
  for (Int_t iev = 0; iev < 1000; iev++) {
    for (Int_t d = 0; d < dim-1; d++) {
      x[d] = xmin[d] + (xmax[d] - xmin[d])*gRandom->Rndm();
    }
    x[dim-1] = gRandom->Gaus(x[0],1) + gRandom->Gaus(x[1],2);
    sp->Fill(x);
  }
  TFile *f = new TFile("sp.root","recreate");
  sp->Write();
}
