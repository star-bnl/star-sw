TGraph *K3() {
  const Int_t n = 10;
  Double_t x[n] = {0.,  1.5, 2.0, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0};
  Double_t y[n] = {1., .436,.355,.237, .20, .17,.149, .13,.114, .1};
  return gr = new TGraph(n,x,y);
}
