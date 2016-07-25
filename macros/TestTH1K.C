void TestTH1K() {
  TH1 *hpx[2];
  hpx[0]    = new TH1F("hp0","Normal histogram",800,-400,400);
  hpx[1]    = new TH1K("hk1","Nearest Neighboor of order 32",800,-400,400,32);
  Int_t   nn[6] = {  200,200,200,20, 200, 200};
  Double_t z[6] = { -200, -51, -45,  0,15, 100};
  Double_t s[6] = {   10,   8,  10, 10,20,  10};
  Int_t N = 300;
  for (Int_t i = 0; i < 6; i++) {
    Int_t n = nn[i];
    for (Int_t j = 0; j < n; j++) {
      Float_t x = gRandom->Gaus(z[i],s[i]);
      hpx[0]->Fill(x);
      hpx[1]->Fill(x);
    }
  }
  Int_t npeaks = 10;
  TSpectrum *sp = new TSpectrum(2*npeaks);
  Int_t nfound = sp->Search(hpx[1]);
}
