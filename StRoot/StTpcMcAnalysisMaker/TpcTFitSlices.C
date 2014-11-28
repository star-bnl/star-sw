void TpcTFitSlices(const Char_t *name = "OuterPadRc") {
  TProfile2D *h = (TProfile2D *) gDirectory->Get(name);
  if (! h) return;
  TF1 *ga = new TF1("ga","exp([0]-x*x/(2*[1]))+[2]",-2.,2.);
  ga->SetParameters(0,1,0);
  ga->SetParLimits(1,0.1,100);
  h->FitSlicesX(ga,0,0,10,"r");
}
