void quantiles(TH1 *h, Int_t nq = 100, Int_t opt=0) {
  if (! h) return;
  if (opt == 0) {
    TH1F *hh = new TH1F(*(TH1F *) h);
    hh->SetName(Form("Qua_%s",h->GetName()));
    h = hh;
    Int_t nx = h->GetNbinsX();
    for (Int_t i = 0; i <= nx; i++) {
      h->SetBinContent(i,h->GetBinContent(i)*h->GetBinCenter(i));
    }
  }
  // demo for quantiles
  Double_t *xq = new Double_t[nq+2];  // position where to compute the quantiles in [0,1]
  Double_t *yq = new Double_t[nq+2];  // array to contain the quantiles
  for (Int_t i=0;i<=nq;i++) xq[i] = Float_t(i)/nq;
  h->GetQuantiles(nq+2,yq,xq);
  
  for (Int_t i = 0; i<= nq; i++) {
    Double_t YQ = 0;
    if (opt == 0 || opt == 3) {YQ = yq[i];}
    else          {YQ = TMath::Sign(1.,yq[i])*0.1*TMath::Exp(TMath::Abs(yq[i]));}
    cout << i << "\tx " << xq[i] << "\ty " << yq[i] << "\tYQ " << YQ << endl;
  }
  for (Int_t i = 0; i <= nq; i++) {
    if (i%10 == 0) cout << endl;
    //    cout << Form("%6.3f,", yq[i]);
    cout << Form("%5.2f,", yq[i]);
  }
  cout << endl;
  delete [] xq;
  delete [] yq;
}
