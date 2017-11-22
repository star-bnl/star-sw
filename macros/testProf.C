void testProf() {
  TProfile *p1 = new TProfile("Mc","Mc",100,-5,5);    
  p1->SetMarkerStyle(20);
  TProfile *p2 = new TProfile("Rc","Rc",100,-5,5);
  p2->SetMarkerStyle(20);
  p2->SetMarkerColor(2);
  Double_t sigma = 1;
  for (Int_t eve = 0; eve < 1000; eve++) {
    Double_t x = 2*gRandom->Rndm();
    Double_t adcs[11];
    Double_t xs[11];
    Double_t sum = 0;
    Double_t xrx = 0;
    for (Int_t i = -5; i <= 5; i++) {
      Double_t xx = i;
      Double_t t1 = (xx - 0.5 - x)/sigma;
      Double_t t2 = (xx + 0.5 - x)/sigma;
      Double_t dd = TMath::Erfc(t1) - TMath::Erfc(t2);
      p1->Fill(xx-x, dd);
      adcs[i+5] = dd;
      xs[i+5]   = xx;
      sum += dd;
      xrx += xx*dd;
    }
    xrx /= sum;
    for (Int_t i = -5; i <= 5; i++) {
      p2->Fill(xs[i+5]-xrx, adcs[i+5]);
    }
  }
}
